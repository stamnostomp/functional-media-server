module FMS.Modules.Library.Routes
  ( libraryRoutes
  , itemsHandler
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), eitherDecode, encode, object, withObject, (.:), (.:?), (.=))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (status201, urlDecode)
import Network.Wai (Request, Response, queryString, responseLBS, strictRequestBody)
import Text.Read (readMaybe)

import FMS.Core.Module (Route, Segment(..), delete, get, jsonResponse, post)
import FMS.Core.Monad (AppError(..), AppM)
import FMS.Core.Query (applyItemQuery, filterBySeries, queryParams, showsFromItems, sortEpisodes)
import FMS.Core.Types.MediaFolder (MediaFolder (..))
import FMS.Modules.Auth.Monad (currentUserFromReq, getItemWithAccess, visibleItems)
import FMS.Modules.Library.Monad (MonadLibrary(..))

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
-- query params @search@, @sortBy@, @sortOrder@, @startIndex@, @limit@ over
-- the whole library, applied by the pure helpers in "FMS.Core.Query".
-- Responds with the envelope
-- @{\"Items\": [...], \"TotalRecordCount\": n, \"StartIndex\": s}@ where
-- the count is taken /before/ pagination. Invalid integer params are
-- ignored (defaults apply). Unlike @GET \/library@, which stays a plain
-- JSON array, this is the paginated envelope Jellyfin clients expect.
itemsHandler :: [(Text, Text)] -> Request -> AppM Response
itemsHandler _captures req = do
  mUser <- currentUserFromReq req
  items <- visibleItems mUser <$> listItems
  let qparams = queryParams (queryString req)
      (total, page) = applyItemQuery qparams items
      startIndex = fromMaybe (0 :: Int) (lookup "startIndex" qparams >>= readMaybe . T.unpack)
  pure (jsonResponse (object
    [ "Items" .= page
    , "TotalRecordCount" .= total
    , "StartIndex" .= startIndex
    ]))

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
