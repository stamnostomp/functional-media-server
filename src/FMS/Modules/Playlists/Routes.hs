module FMS.Modules.Playlists.Routes
  ( playlistsRoutes
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), eitherDecode, encode, object, withObject, (.:), (.:?), (.!=), (.=))
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types (status201)
import Network.Wai (Request, Response, responseLBS, strictRequestBody)

import FMS.Core.Module (Route, Segment (..), delete, get, jsonResponse, post)
import FMS.Core.Monad (AppError (..), AppM)
import FMS.Modules.Library (MonadLibrary (..))
import FMS.Modules.Playlists.Monad (MonadPlaylists (..))
import FMS.Modules.Playlists.Types (Playlist (..))

-- | The playlists API (docs/todo.org section 10, milestone 10a),
-- Jellyfin-shaped: list/create/get/delete playlists plus per-playlist
-- item add/remove/list. All responses are JSON; bad bodies are 400,
-- unknown playlists or items are 404.
playlistsRoutes :: [Route]
playlistsRoutes =
  [ get    [Lit "Playlists"] listPlaylistsHandler
  , post   [Lit "Playlists"] createPlaylistHandler
  , get    [Lit "Playlists", Capture "id"] getPlaylistHandler
  , delete [Lit "Playlists", Capture "id"] deletePlaylistHandler
  , get    [Lit "Playlists", Capture "id", Lit "Items"] playlistItemsHandler
  , post   [Lit "Playlists", Capture "id", Lit "Items", Capture "itemId"] addItemHandler
  , delete [Lit "Playlists", Capture "id", Lit "Items", Capture "itemId"] removeItemHandler
  ]

-- | Body of @POST /Playlists@: @{\"Name\": \"...\", \"ItemIds\"?: [\"1\", ...]}@.
data CreatePlaylistRequest = CreatePlaylistRequest
  { cprName    :: Text
  , cprItemIds :: [Text]
  }

instance FromJSON CreatePlaylistRequest where
  parseJSON = withObject "CreatePlaylistRequest" $ \o ->
    CreatePlaylistRequest
      <$> o .:  "Name"
      <*> o .:? "ItemIds" .!= []

-- Handlers ------------------------------------------------------------------

-- | @GET /Playlists@ — 200 JSON array of all playlists (oldest first).
listPlaylistsHandler :: [(Text, Text)] -> Request -> AppM Response
listPlaylistsHandler _captures _req =
  jsonResponse <$> listPlaylists

-- | @POST /Playlists@ — create a playlist. 400 on a malformed body or an
-- empty name; unknown item ids are dropped by 'createPlaylist'. Otherwise
-- 201 with the new playlist's JSON.
createPlaylistHandler :: [(Text, Text)] -> Request -> AppM Response
createPlaylistHandler _captures req = do
  body <- liftIO (strictRequestBody req)
  case eitherDecode body of
    Left err -> throwError (BadRequest ("invalid JSON body: " <> T.pack err))
    Right cpr
      | T.null (T.strip (cprName cpr)) ->
          throwError (BadRequest "playlist name must not be empty")
      | otherwise -> do
          p <- createPlaylist (cprName cpr) (cprItemIds cpr)
          pure (responseLBS status201
                  [("Content-Type", "application/json")] (encode p))

-- | @GET /Playlists/:id@ — 200 the playlist JSON, 404 when unknown.
getPlaylistHandler :: [(Text, Text)] -> Request -> AppM Response
getPlaylistHandler captures _req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just pid -> do
      mP <- getPlaylist pid
      case mP of
        Nothing -> throwError (NotFound ("no playlist with id " <> pid))
        Just p  -> pure (jsonResponse p)

-- | @DELETE /Playlists/:id@ — 200 @{\"status\":\"ok\"}@, 404 when unknown.
deletePlaylistHandler :: [(Text, Text)] -> Request -> AppM Response
deletePlaylistHandler captures _req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just pid -> do
      ok <- deletePlaylist pid
      if ok
        then pure (jsonResponse (object ["status" .= ("ok" :: Text)]))
        else throwError (NotFound ("no playlist with id " <> pid))

-- | @GET /Playlists/:id/Items@ — 200 the playlist's library items as a
-- JSON array, in playlist order (ids whose item no longer exists are
-- skipped). 404 when the playlist is unknown.
playlistItemsHandler :: [(Text, Text)] -> Request -> AppM Response
playlistItemsHandler captures _req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just pid -> do
      mP <- getPlaylist pid
      case mP of
        Nothing -> throwError (NotFound ("no playlist with id " <> pid))
        Just p  -> do
          mItems <- mapM getItem (plItemIds p)
          pure (jsonResponse (catMaybes mItems))

-- | @POST /Playlists/:id/Items/:itemId@ — add the item to the playlist
-- (deduped). 404 when the playlist or the item is unknown; otherwise 200
-- with the updated playlist JSON.
addItemHandler :: [(Text, Text)] -> Request -> AppM Response
addItemHandler captures _req =
  case (lookup "id" captures, lookup "itemId" captures) of
    (Nothing, _) -> throwError (BadRequest "missing id capture")
    (_, Nothing) -> throwError (BadRequest "missing itemId capture")
    (Just pid, Just itemId) -> do
      mP <- getPlaylist pid
      case mP of
        Nothing -> throwError (NotFound ("no playlist with id " <> pid))
        Just _ -> do
          mItem <- getItem itemId
          case mItem of
            Nothing -> throwError (NotFound ("no library item with id " <> itemId))
            Just _ -> do
              _ <- addToPlaylist pid itemId
              mUpdated <- getPlaylist pid
              case mUpdated of
                Just p  -> pure (jsonResponse p)
                Nothing -> throwError (InternalError "playlist vanished after update")

-- | @DELETE /Playlists/:id/Items/:itemId@ — remove the item from the
-- playlist. 404 when the playlist or the item is unknown; otherwise 200
-- with the updated playlist JSON.
removeItemHandler :: [(Text, Text)] -> Request -> AppM Response
removeItemHandler captures _req =
  case (lookup "id" captures, lookup "itemId" captures) of
    (Nothing, _) -> throwError (BadRequest "missing id capture")
    (_, Nothing) -> throwError (BadRequest "missing itemId capture")
    (Just pid, Just itemId) -> do
      mP <- getPlaylist pid
      case mP of
        Nothing -> throwError (NotFound ("no playlist with id " <> pid))
        Just _ -> do
          mItem <- getItem itemId
          case mItem of
            Nothing -> throwError (NotFound ("no library item with id " <> itemId))
            Just _ -> do
              _ <- removeFromPlaylist pid itemId
              mUpdated <- getPlaylist pid
              case mUpdated of
                Just p  -> pure (jsonResponse p)
                Nothing -> throwError (InternalError "playlist vanished after update")
