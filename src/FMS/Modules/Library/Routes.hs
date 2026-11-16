module FMS.Modules.Library.Routes
  ( libraryRoutes
  , itemsHandler
  , itemsCountsHandler
  , itemDetailHandler
  , ancestorsHandler
  , searchHintsHandler
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), ToJSON (..), eitherDecode, encode, object, withObject, (.:), (.:?), (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.HashMap.Strict as HM
import Data.List (sortBy)
import Data.Map.Strict (toAscList)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (status201, urlDecode)
import Network.Wai (Request, Response, queryString, responseLBS, strictRequestBody)
import Text.Read (readMaybe)

import FMS.Core.Module (Route, Segment(..), delete, get, jsonResponse, post)
import FMS.Core.Monad (AppError(..), AppM)
import FMS.Core.Query (applyItemQuery, applyUserDataFilters, attachUserData, filterBySeries, projectFields, queryParams, searchItems, showsFromItems, sortEpisodes)
import FMS.Core.Types.MediaFolder (MediaFolder (..))
import FMS.Core.Types.MediaItem (MediaItem (..))
import FMS.Core.Types.User (User (..))
import FMS.Modules.Auth.Monad (currentUserFromReq, getItemWithAccess, visibleItems)
import FMS.Modules.Library.Monad (MonadLibrary(..))
import FMS.Modules.UserData.Monad (MonadUserData (..))
import FMS.Modules.UserData.Types (UserData)

-- | The library API (docs/modules.org): list all items, fetch one by id,
-- trigger a rescan, and (milestone 2c) manage the media folders
-- (@/Library/MediaFolders@, Jellyfin-shaped).
libraryRoutes :: [Route]
libraryRoutes =
  [ get    [Lit "library"] listItemsHandler
  , get    [Lit "library", Capture "id"] itemHandler
  , post   [Lit "library", Lit "rescan"] rescanHandler
  , get    [Lit "Library", Lit "MediaFolders"] listFoldersHandler
  , post   [Lit "Library", Lit "MediaFolders"] addFolderHandler
  , delete [Lit "Library", Lit "MediaFolders", Capture "id"] removeFolderHandler
  , get    [Lit "Shows"] showsHandler
  , get    [Lit "Shows", Capture "name", Lit "Episodes"] episodesHandler
  , get    [Lit "Shows", Capture "name", Lit "NextUp"] nextUpHandler
  ]

-- | @GET \/library@ — JSON array of all items visible to the requester
-- (milestone 5d: a valid non-admin token filters to the user's allowed
-- folders; anonymous/open mode sees everything).
listItemsHandler :: [(Text, Text)] -> Request -> AppM Response
listItemsHandler _captures req = do
  mUser <- currentUserFromReq req
  jsonResponse <$> (visibleItems mUser <$> listItems)

-- | @GET \/items@ — Jellyfin-shaped items query (docs/todo.org section 4):
-- query params @search@/@searchTerm@, @sortBy@, @sortOrder@, @startIndex@,
-- @limit@ over the whole library, applied by the pure helpers in
-- "FMS.Core.Query". Milestone 5e: @userId@ scopes the query to that
-- user's 'UserData' (attached as a @UserData@ key on each item),
-- @filters=IsPlayed,Unplayed,...@ filters on it (applied BEFORE the
-- count/pagination so @TotalRecordCount@ reflects them), and @fields=a,b@
-- projects the item JSON keys. A non-admin user asking for another
-- user's @userId@ gets 403. Responds with the envelope
-- @{\"Items\": [...], \"TotalRecordCount\": n, \"StartIndex\": s}@.
-- Invalid integer params are ignored (defaults apply). Unlike
-- @GET \/library@, which stays a plain JSON array, this is the paginated
-- envelope Jellyfin clients expect.
itemsHandler :: [(Text, Text)] -> Request -> AppM Response
itemsHandler _captures req = do
  mUser <- currentUserFromReq req
  items0 <- visibleItems mUser <$> listItems
  let qparams = queryParams (queryString req)
  (mLookup, filterTokens, fields) <- resolveUserQuery qparams mUser
  let items = case mLookup of
        Just lk | not (null filterTokens) -> applyUserDataFilters filterTokens lk items0
        _                                 -> items0
      (total, page) = applyItemQuery qparams items
      startIndex = fromMaybe (0 :: Int) (lookup "startIndex" qparams >>= readMaybe . T.unpack)
      renderItem i = projectFields fields (attachUserData mLookup i (toJSON i))
  pure (jsonResponse (object
    [ "Items" .= map renderItem page
    , "TotalRecordCount" .= total
    , "StartIndex" .= startIndex
    ]))

-- | Milestone 5e shared helper for @\/items@ and @\/items\/:id@: resolve the
-- optional @userId@ query param into a 'UserData' lookup (403 'Forbidden'
-- when a non-admin user asks for a different user's data; admins and
-- anonymous/open mode pass), and parse @filters@ (comma-separated tokens)
-- and @fields@ (comma-separated keys) from the query string.
resolveUserQuery
  :: [(Text, Text)]
  -> Maybe User
  -> AppM ( Maybe (Text -> Maybe UserData)  -- UserData lookup by itemId
            , [Text]                         -- filters tokens
            , [Text]                         -- fields keys
            )
resolveUserQuery qparams mUser = do
  mLookup <- case lookup "userId" qparams of
    Nothing -> pure Nothing
    Just uid -> do
      case mUser of
        Just u | not (isAdmin u), userId u /= uid ->
          throwError (Forbidden ("userId " <> uid <> " does not match the authenticated user"))
        _ -> pure ()
      ud <- listUserData uid
      pure (Just (flip HM.lookup ud))
  let filterTokens = case lookup "filters" qparams of
        Just f  -> [ t | t <- T.splitOn "," f, not (T.null (T.strip t)) ]
        Nothing -> []
      fields = case lookup "fields" qparams of
        Just f | not (T.null (T.strip f)) -> T.splitOn "," f
        _                                 -> []
  pure (mLookup, filterTokens, fields)

-- | @GET \/library\/:id@ — one item, or 404. Milestone 5d: 401 when the
-- requester's token belongs to a non-admin user without access to the
-- item's folder.
itemHandler :: [(Text, Text)] -> Request -> AppM Response
itemHandler captures req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just i  -> do
      mItem <- getItemWithAccess req i
      case mItem of
        Nothing -> throwError (NotFound ("no library item with id " <> i))
        Just item -> pure (jsonResponse item)

-- | @POST \/library\/rescan@ — rescan the library (all media folders),
-- then @{\"status\":\"ok\"}@.
rescanHandler :: [(Text, Text)] -> Request -> AppM Response
rescanHandler _captures _req = do
  rescanLibrary
  pure (jsonResponse (object ["status" .= ("ok" :: Text)]))

-- Media folders (milestone 2c) -------------------------------------------------

-- | Body of @POST /Library/MediaFolders@:
-- @{\"Name\": \"Movies\", \"Path\": \"/mnt/movies\", \"Type\"?: \"movies\"@}@.
data AddFolderRequest = AddFolderRequest
  { afrName :: Text
  , afrPath :: FilePath
  , afrType :: Maybe Text
  }

instance FromJSON AddFolderRequest where
  parseJSON = withObject "AddFolderRequest" $ \o ->
    AddFolderRequest
      <$> o .:  "Name"
      <*> o .:  "Path"
      <*> o .:? "Type"

-- | @GET \/Library\/MediaFolders@ — 200 JSON array of the folders
-- (Id/Name/Path/Type), sorted by id.
listFoldersHandler :: [(Text, Text)] -> Request -> AppM Response
listFoldersHandler _captures _req =
  jsonResponse <$> listFolders

-- | @POST \/Library\/MediaFolders@ — create a folder (the path is created
-- if missing, the type defaults to \"mixed\"), rescan so its items appear,
-- then 201 with the folder JSON. Bad JSON -> 400.
addFolderHandler :: [(Text, Text)] -> Request -> AppM Response
addFolderHandler _captures req = do
  body <- liftIO (strictRequestBody req)
  case eitherDecode body of
    Left err -> throwError (BadRequest ("invalid JSON body: " <> T.pack err))
    Right afr -> do
      f <- addFolder (afrName afr) (afrPath afr) (fromMaybe "mixed" (afrType afr))
      rescanLibrary
      pure (responseLBS status201 [("Content-Type", "application/json")] (encode f))

-- | @DELETE \/Library\/MediaFolders\/:id@ — remove the folder and its items
-- from acid-state; 200 @{\"status\":\"ok\"}@, 404 for an unknown id, 400
-- when trying to remove the last remaining folder.
removeFolderHandler :: [(Text, Text)] -> Request -> AppM Response
removeFolderHandler captures _req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just fid -> do
      folders <- listFolders
      if length folders <= 1 && any ((== fid) . folderId) folders
        then throwError (BadRequest "cannot remove the last media folder")
        else do
          ok <- removeFolder fid
          if ok
            then pure (jsonResponse (object ["status" .= ("ok" :: Text)]))
            else throwError (NotFound ("no media folder with id " <> fid))

-- TV shows (milestone 4d) -----------------------------------------------------

-- | Percent-decode a path capture (series names may contain spaces etc.).
decodeCapture :: Text -> Text
decodeCapture = TE.decodeUtf8 . urlDecode True . TE.encodeUtf8

-- | @GET \/Shows@ — 200 array of @{\"Name\": series, \"ItemCount\": n}@,
-- sorted by name (pure 'showsFromItems').
showsHandler :: [(Text, Text)] -> Request -> AppM Response
showsHandler _captures req = do
  mUser <- currentUserFromReq req
  items <- visibleItems mUser <$> listItems
  pure (jsonResponse [ object ["Name" .= s, "ItemCount" .= n]
                     | (s, n) <- showsFromItems items ])

-- | @GET \/Shows\/:name\/Episodes@ — 200 array of the series' episodes
-- ordered by (season, episode, title) (pure 'sortEpisodes'); an unknown
-- series is an empty array.
episodesHandler :: [(Text, Text)] -> Request -> AppM Response
episodesHandler captures req = do
  name <- decodeCapture <$> captureName captures
  mUser <- currentUserFromReq req
  items <- visibleItems mUser <$> listItems
  let eps = sortEpisodes (filterBySeries (Just name) items)
  pure (jsonResponse eps)

-- | @GET \/Shows\/:name\/NextUp@ — 200 the first episode by the
-- (season, episode, title) order, 404 when the series has no episodes.
nextUpHandler :: [(Text, Text)] -> Request -> AppM Response
nextUpHandler captures req = do
  name <- decodeCapture <$> captureName captures
  mUser <- currentUserFromReq req
  items <- visibleItems mUser <$> listItems
  case sortEpisodes (filterBySeries (Just name) items) of
    []   -> throwError (NotFound ("no episodes for series " <> name))
    (e : _) -> pure (jsonResponse e)

captureName :: [(Text, Text)] -> AppM Text
captureName captures =
  case lookup "name" captures of
    Nothing -> throwError (BadRequest "missing name capture")
    Just n  -> pure n

-- Milestone 4e: item detail / ancestors / counts / search hints -------------
-- NOTE on registration order (FMS.Modules.Library): routes are first-match
-- wins, so [Lit "items", Lit "Counts"] must be registered BEFORE
-- [Lit "items", Capture "id"].

-- | @GET \/items\/:id@ — one item, or 404. Same behaviour as
-- @GET \/library\/:id@ (milestone 5d folder enforcement via
-- 'getItemWithAccess'), on the Jellyfin-shaped /items path. Milestone 5e:
-- @userId@ attaches that user's @UserData@ to the item JSON (403 for a
-- non-admin asking for another user) and @fields@ projects the keys.
itemDetailHandler :: [(Text, Text)] -> Request -> AppM Response
itemDetailHandler captures req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just i  -> do
      mItem <- getItemWithAccess req i
      case mItem of
        Nothing -> throwError (NotFound ("no library item with id " <> i))
        Just item -> do
          mUser <- currentUserFromReq req
          let qparams = queryParams (queryString req)
          (mLookup, _filterTokens, fields) <- resolveUserQuery qparams mUser
          pure (jsonResponse (projectFields fields (attachUserData mLookup item (toJSON item))))

-- | @GET \/items\/Counts@ — JSON object mapping item Type -> count over
-- the requester-visible items, e.g. @{\"Movie\":2,\"Episode\":5}@.
itemsCountsHandler :: [(Text, Text)] -> Request -> AppM Response
itemsCountsHandler _captures req = do
  mUser <- currentUserFromReq req
  items <- visibleItems mUser <$> listItems
  let counts = toAscList (Map.fromListWith (+) [(itemType i, 1 :: Int) | i <- items])
  pure (jsonResponse (object [ Key.fromText k .= n | (k, n) <- counts ]))

-- | @GET \/items\/:id\/Ancestors@ — breadcrumb array: the item's media
-- folder as @{\"Name\": folderName, \"Type\": \"CollectionFolder\"}@, then
-- for episodes @{\"Name\": series, \"Type\": \"Series\"}@, then the item
-- itself. 404 when the item is unknown (401 when the folder is denied).
ancestorsHandler :: [(Text, Text)] -> Request -> AppM Response
ancestorsHandler captures req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just i  -> do
      mItem <- getItemWithAccess req i
      case mItem of
        Nothing -> throwError (NotFound ("no library item with id " <> i))
        Just item -> do
          folders <- listFolders
          let folderCrumb = case [ f | f <- folders, folderId f == itemFolder item ] of
                (f : _) -> Just (object [ "Name" .= folderName f
                                        , "Type" .= ("CollectionFolder" :: Text) ])
                []      -> Nothing
              seriesCrumb = case (itemType item, itemSeries item) of
                ("Episode", Just s) ->
                  Just (object [ "Name" .= s, "Type" .= ("Series" :: Text) ])
                _ -> Nothing
          pure (jsonResponse (catMaybes [folderCrumb, seriesCrumb] ++ [toJSON item]))

-- | @GET \/Search\/Hints@ — query params @searchTerm@ (alias @term@) and
-- @limit@ (default 20): an array of @{\"ItemId\",\"Name\",\"Type\"@ plus
-- @\"Series\"@ when the item has one@}@ for the visible items whose title
-- contains the term case-insensitively, sorted by Name and capped at
-- @limit@. An empty term yields an empty array.
searchHintsHandler :: [(Text, Text)] -> Request -> AppM Response
searchHintsHandler _captures req = do
  mUser <- currentUserFromReq req
  items <- visibleItems mUser <$> listItems
  let qparams = queryParams (queryString req)
      term = case lookup "searchTerm" qparams of
        Just t  -> t
        Nothing -> fromMaybe "" (lookup "term" qparams)
      limit = fromMaybe (20 :: Int) (lookup "limit" qparams >>= readMaybe . T.unpack)
      hits = if T.null term
               then []
               else take limit (sortBy (comparing itemTitle) (searchItems term items))
  pure (jsonResponse
    [ object ([ "ItemId" .= itemId i, "Name" .= itemTitle i, "Type" .= itemType i ]
              ++ maybe [] (\s -> ["Series" .= s]) (itemSeries i))
    | i <- hits ])
