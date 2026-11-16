module FMS.Modules.Library.Routes
  ( libraryRoutes
  , itemsHandler
  , itemsQueryResponse
  , itemsCountsHandler
  , itemDetailHandler
  , ancestorsHandler
  , searchHintsHandler
  , specialFeaturesHandler
  ) where

import Control.Concurrent.STM (TVar, readTVarIO)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, eitherDecode, encode, object, withObject, (.:), (.:?), (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
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
import FMS.Core.Monad (AppError(..), AppM, getState)
import FMS.Core.Query (applyItemQuery, applyMetadataFilters, applyUserDataFilters, attachUserData, filterBySeries, isUserDataSortKey, projectFields, queryParams, searchItems, showsFromItems, sortByUserDataField, sortEpisodes, specialFeaturesOf, typeFilterIncludesBoxSet)
import FMS.Core.Types.MediaFolder (MediaFolder (..))
import FMS.Core.Types.MediaItem (MediaItem (..))
import FMS.Core.Types.User (User (..))
import FMS.Modules.Auth.Monad (currentUserFromReq, getItemWithAccess, visibleItems)
import FMS.Modules.Collections.Types (Collection (..), boxSetIdPrefix, boxSetItemValue, isBoxSetId)
import FMS.Modules.Library.Monad (MonadLibrary(..))
import FMS.Modules.Metadata.Types (MediaMetadata)
import FMS.Modules.Socket (broadcastSocket)
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
  jsonResponse <$> (listItems >>= visibleItems mUser)

-- | @GET \/items@ — Jellyfin-shaped items query (docs/todo.org section 4):
-- query params @search@/@searchTerm@, @sortBy@, @sortOrder@, @startIndex@,
-- @limit@ over the whole library, applied by the pure helpers in
-- "FMS.Core.Query". Milestone 5e: @userId@ scopes the query to that
-- user's 'UserData' (attached as a @UserData@ key on each item),
-- @filters=IsPlayed,Unplayed,...@ filters on it, and @fields=a,b@ projects
-- the item JSON keys. Milestone 4h: @genres@/@studios@/@persons@ filter on
-- the stored metadata. The user-data and metadata filters are applied
-- BEFORE the count/pagination so @TotalRecordCount@ reflects them. A
-- non-admin user asking for another user's @userId@ gets 403. Responds
-- with the envelope
-- @{\"Items\": [...], \"TotalRecordCount\": n, \"StartIndex\": s}@.
-- Invalid integer params are ignored (defaults apply). Unlike
-- @GET \/library@, which stays a plain JSON array, this is the paginated
-- envelope Jellyfin clients expect.
itemsHandler :: [(Text, Text)] -> Request -> AppM Response
itemsHandler _captures req = do
  mUser <- currentUserFromReq req
  itemsQueryResponse mUser (queryParams (queryString req))

-- | Milestone 5f: the shared body of @GET /items@ and
-- @GET /Users/:userId/Items@ — 'itemsHandler' passes the request's own
-- user and query string, the watch-history route passes the path's user
-- with @userId@ forced into the params.
itemsQueryResponse :: Maybe User -> [(Text, Text)] -> AppM Response
itemsQueryResponse mUser qparams = do
  items0 <- listItems >>= visibleItems mUser
  (mLookup, filterTokens, fields) <- resolveUserQuery qparams mUser
  -- Milestone 4h: the metadata cache map keyed by itemId. Read straight
  -- from the typed registry under "metadata.cache" (the same key as
  -- FMS.Modules.Metadata.Monad.metadataStateKey) because importing
  -- FMS.Modules.Metadata.Monad here would create a module cycle
  -- (Metadata.Monad -> Library -> Library.Routes).
  mMetaTv <- getState "metadata.cache" :: AppM (Maybe (TVar (HashMap Text MediaMetadata)))
  metaMap <- maybe (pure HM.empty) (liftIO . readTVarIO) mMetaTv
  -- Milestone 7: when the effective type filter includes BoxSet, the
  -- synthesized BoxSet pseudo-items (collections visible to the
  -- requester) are appended after the page and counted in the total.
  boxVals <- if typeFilterIncludesBoxSet qparams
               then boxSetValuesFor mUser items0
               else pure []
  let metaLookup = flip HM.lookup metaMap
      items1 = applyMetadataFilters qparams metaLookup items0
      itemsU = case mLookup of
        Just lk | not (null filterTokens) -> applyUserDataFilters filterTokens lk items1
        _                                 -> items1
      -- Milestone 5f: when userId is present and sortBy is a user-data key
      -- (lastPlayedDate/playCount), pre-sort by that field; applyItemQuery
      -- then skips its own sort for these keys, so pagination sees it.
      items = case (mLookup, lookup "sortBy" qparams) of
        (Just lk, Just s) | isUserDataSortKey s -> sortByUserDataField s lk itemsU
        _                                       -> itemsU
      (total, page) = applyItemQuery qparams items
      startIndex = fromMaybe (0 :: Int) (lookup "startIndex" qparams >>= readMaybe . T.unpack)
      renderItem i = projectFields fields (attachUserData mLookup i (toJSON i))
  pure (jsonResponse (object
    [ "Items" .= (map renderItem page ++ map (projectFields fields) boxVals)
    , "TotalRecordCount" .= (total + length boxVals)
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
-- then @{\"status\":\"ok\"}@. Milestone 8: broadcasts @LibraryChanged@ to
-- all WebSocket clients after the rescan.
rescanHandler :: [(Text, Text)] -> Request -> AppM Response
rescanHandler _captures _req = do
  rescanLibrary
  _ <- broadcastSocket "LibraryChanged" (object [])
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
  items <- listItems >>= visibleItems mUser
  pure (jsonResponse [ object ["Name" .= s, "ItemCount" .= n]
                     | (s, n) <- showsFromItems items ])

-- | @GET \/Shows\/:name\/Episodes@ — 200 array of the series' episodes
-- ordered by (season, episode, title) (pure 'sortEpisodes'); an unknown
-- series is an empty array.
episodesHandler :: [(Text, Text)] -> Request -> AppM Response
episodesHandler captures req = do
  name <- decodeCapture <$> captureName captures
  mUser <- currentUserFromReq req
  items <- listItems >>= visibleItems mUser
  let eps = sortEpisodes (filterBySeries (Just name) items)
  pure (jsonResponse eps)

-- | @GET \/Shows\/:name\/NextUp@ — 200 the first episode by the
-- (season, episode, title) order, 404 when the series has no episodes.
nextUpHandler :: [(Text, Text)] -> Request -> AppM Response
nextUpHandler captures req = do
  name <- decodeCapture <$> captureName captures
  mUser <- currentUserFromReq req
  items <- listItems >>= visibleItems mUser
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
    Just i
      -- Milestone 7: a "boxset-<colId>" id resolves to the collection's
      -- pseudo-item; normal ids go through 'getItemWithAccess' unchanged.
      | isBoxSetId i -> boxSetDetailHandler i req
      | otherwise -> do
          mItem <- getItemWithAccess req i
          case mItem of
            Nothing -> throwError (NotFound ("no library item with id " <> i))
            Just item -> do
              mUser <- currentUserFromReq req
              let qparams = queryParams (queryString req)
              (mLookup, _filterTokens, fields) <- resolveUserQuery qparams mUser
              pure (jsonResponse (projectFields fields (attachUserData mLookup item (toJSON item))))

-- | Milestone 7: @GET \/items\/boxset-<colId>@ — the collection's BoxSet
-- pseudo-item JSON; 404 when the collection does not exist or is not
-- visible to the requester (all its items are in denied folders).
boxSetDetailHandler :: Text -> Request -> AppM Response
boxSetDetailHandler i req = do
  cm <- collectionsMap
  let cid = T.drop (T.length boxSetIdPrefix) i
  case HM.lookup cid cm of
    Nothing -> throwError (NotFound ("no library item with id " <> i))
    Just c  -> do
      mUser <- currentUserFromReq req
      visible <- listItems >>= visibleItems mUser
      if collectionVisibleTo mUser visible c
        then pure (jsonResponse (boxSetItemValue c))
        else throwError (NotFound ("no library item with id " <> i))

-- | @GET \/items\/Counts@ — JSON object mapping item Type -> count over
-- the requester-visible items, e.g. @{\"Movie\":2,\"Episode\":5}@.
itemsCountsHandler :: [(Text, Text)] -> Request -> AppM Response
itemsCountsHandler _captures req = do
  mUser <- currentUserFromReq req
  items <- listItems >>= visibleItems mUser
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

-- Milestone 7: BoxSet pseudo-items (collections) ------------------------------

-- | The collections map from the typed registry under "collections.map"
-- (the same key as
-- FMS.Modules.Collections.Monad.collectionsStateKey; a literal here
-- because importing Collections.Monad would create a module cycle,
-- Collections.Monad -> Library -> Library.Routes).
collectionsMap :: AppM (HashMap Text Collection)
collectionsMap = do
  mTv <- getState "collections.map" :: AppM (Maybe (TVar (HashMap Text Collection)))
  maybe (pure HM.empty) (liftIO . readTVarIO) mTv

-- | Milestone 7: is the collection (box set) visible to the requester?
-- Admins and anonymous (open mode) see all collections; a restricted
-- non-admin user sees a box set only when at least one of its item ids is
-- visible to them.
collectionVisibleTo :: Maybe User -> [MediaItem] -> Collection -> Bool
collectionVisibleTo mUser visItems c = case mUser of
  Just u | not (isAdmin u) ->
    let visIds = HM.fromList [(itemId i, ()) | i <- visItems]
    in any (`HM.member` visIds) (colItemIds c)
  _ -> True

-- | The BoxSet pseudo-item JSON values of all collections visible to the
-- requester (used by 'itemsHandler' when the type filter includes BoxSet).
boxSetValuesFor :: Maybe User -> [MediaItem] -> AppM [Value]
boxSetValuesFor mUser visible = do
  cm <- collectionsMap
  pure [ boxSetItemValue c | c <- HM.elems cm, collectionVisibleTo mUser visible c ]

-- | @GET \/Search\/Hints@ — query params @searchTerm@ (alias @term@) and
-- @limit@ (default 20): an array of @{\"ItemId\",\"Name\",\"Type\"@ plus
-- @\"Series\"@ when the item has one@}@ for the visible items whose title
-- contains the term case-insensitively, sorted by Name and capped at
-- @limit@. An empty term yields an empty array.
searchHintsHandler :: [(Text, Text)] -> Request -> AppM Response
searchHintsHandler _captures req = do
  mUser <- currentUserFromReq req
  items <- listItems >>= visibleItems mUser
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

-- | @GET \/items\/:id\/SpecialFeatures@ — 404 for an unknown item
-- (milestone 5d folder enforcement via 'getItemWithAccess'), otherwise a
-- JSON array of the visible Extra items living in the same directory as
-- the item's file or one level below it (pure 'specialFeaturesOf',
-- milestone 4g).
specialFeaturesHandler :: [(Text, Text)] -> Request -> AppM Response
specialFeaturesHandler captures req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just i  -> do
      mItem <- getItemWithAccess req i
      case mItem of
        Nothing -> throwError (NotFound ("no library item with id " <> i))
        Just item -> do
          mUser <- currentUserFromReq req
          items <- listItems >>= visibleItems mUser
          pure (jsonResponse (specialFeaturesOf item items))
