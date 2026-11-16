module FMS.Modules.UserData.Routes
  ( userDataRoutes
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, eitherDecode, object, withObject, (.:), (.=))
import Data.Aeson.Types (parseEither)
import Data.CaseInsensitive (original)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Network.Wai (Request, Response, requestHeaders, strictRequestBody)

import FMS.Core.Module (Route, Segment (..), delete, get, jsonResponse, post)
import FMS.Core.Monad (AppError (..), AppM)
import FMS.Core.Router (parseTokenHeader)
import FMS.Core.Types.MediaItem (MediaItem)
import FMS.Core.Types.User (User (..))
import FMS.Modules.Auth.Monad (MonadAuth (..))
import FMS.Modules.Library (MonadLibrary (..))
import FMS.Modules.Socket (broadcastSocket)
import FMS.Modules.UserData.Monad
  ( MonadUserData (..)
  , markPlayed
  , setFavorite
  )
import FMS.Modules.UserData.Types (UserData (..))

-- | The per-user item data API (docs/todo.org section 9, milestone 5c),
-- Jellyfin-shaped. Every route requires a valid credential
-- (@X-Emby-Token@ / @Authorization@, parsed with 'parseTokenHeader' and
-- resolved with 'MonadAuth'), and the @:userId@ in the path must be the
-- token's own user or the token must be an admin — otherwise 401. The
-- @:itemId@ must exist in the library (404 otherwise).
userDataRoutes :: [Route]
userDataRoutes =
  [ get    [Lit "Users", Capture "userId", Lit "Items", Capture "itemId", Lit "UserData"]
           getUserDataHandler
  , post   [Lit "Users", Capture "userId", Lit "Items", Capture "itemId", Lit "UserData"]
           updateUserDataHandler
  , post   [Lit "Users", Capture "userId", Lit "PlayedItems", Capture "itemId"]
           playedHandler
  , delete [Lit "Users", Capture "userId", Lit "PlayedItems", Capture "itemId"]
           unplayedHandler
  , post   [Lit "Users", Capture "userId", Lit "FavoriteItems", Capture "itemId"]
           favoriteHandler
  , delete [Lit "Users", Capture "userId", Lit "FavoriteItems", Capture "itemId"]
           unfavoriteHandler
  ]

-- | Body of @POST /Users/:userId/PlayedItems/:itemId@ (Jellyfin field name).
data PlayedRequest = PlayedRequest
  { playedReqPlayed :: Bool
  }

instance FromJSON PlayedRequest where
  parseJSON = withObject "PlayedRequest" $ \o ->
    PlayedRequest <$> o .: "Played"

-- Handlers ------------------------------------------------------------------

-- | @GET /Users/:userId/Items/:itemId/UserData@ — the stored record, or the
-- empty default when nothing is stored yet. 200 with the 'UserData' JSON.
getUserDataHandler :: [(Text, Text)] -> Request -> AppM Response
getUserDataHandler captures req = do
  (uid, iid) <- captureUserItem captures
  _ <- requireUserDataAccess req uid
  _ <- requireExistingItem iid
  ud <- getUserData uid iid
  pure (jsonResponse ud)

-- | @POST /Users/:userId/Items/:itemId/UserData@ with a partial JSON body
-- (any subset of @played@ / @favorite@ / @playbackPosition@ / @playCount@ /
-- @lastPlayedAt@). The present fields are merged into the stored record
-- (merge, not replace), persisted, and the full updated record is
-- returned with 200. Malformed JSON or a wrongly typed field -> 400.
updateUserDataHandler :: [(Text, Text)] -> Request -> AppM Response
updateUserDataHandler captures req = do
  (uid, iid) <- captureUserItem captures
  _ <- requireUserDataAccess req uid
  _ <- requireExistingItem iid
  body <- liftIO (strictRequestBody req)
  case eitherDecode body of
    Left err -> throwError (BadRequest ("invalid JSON body: " <> T.pack err))
    Right patch -> do
      existing <- getUserData uid iid
      case mergeUserDataPatch patch existing of
        Left err -> throwError (BadRequest ("invalid UserData field: " <> T.pack err))
        Right merged -> do
          ud <- updateUserData uid iid (const merged)
          _ <- broadcastSocket "UserDataSaved" (object   -- milestone 8
                 [ "UserId"   .= uid
                 , "ItemId"   .= iid
                 , "UserData" .= toJSON ud
                 ])
          pure (jsonResponse ud)

-- | @POST /Users/:userId/PlayedItems/:itemId@ with @{\"Played\": true|false}@ —
-- mark played (bumping the play count and stamping @lastPlayedAt@) or
-- unplayed. 200 with the updated record; 400 on a malformed body.
playedHandler :: [(Text, Text)] -> Request -> AppM Response
playedHandler captures req = do
  (uid, iid) <- captureUserItem captures
  _ <- requireUserDataAccess req uid
  _ <- requireExistingItem iid
  body <- liftIO (strictRequestBody req)
  case eitherDecode body of
    Left err -> throwError (BadRequest ("invalid JSON body: " <> T.pack err))
    Right pr -> do
      now <- liftIO getCurrentTime
      let b = playedReqPlayed pr
      ud <- updateUserData uid iid $ \u ->
        let u' = markPlayed b u
        in if b then u' { udLastPlayedAt = Just now } else u'
      pure (jsonResponse ud)

-- | @DELETE /Users/:userId/PlayedItems/:itemId@ — mark unplayed.
unplayedHandler :: [(Text, Text)] -> Request -> AppM Response
unplayedHandler captures req = do
  (uid, iid) <- captureUserItem captures
  _ <- requireUserDataAccess req uid
  _ <- requireExistingItem iid
  updateUserData uid iid (markPlayed False) >>= pure . jsonResponse

-- | @POST /Users/:userId/FavoriteItems/:itemId@ — mark favorite.
favoriteHandler :: [(Text, Text)] -> Request -> AppM Response
favoriteHandler captures req = do
  (uid, iid) <- captureUserItem captures
  _ <- requireUserDataAccess req uid
  _ <- requireExistingItem iid
  updateUserData uid iid (setFavorite True) >>= pure . jsonResponse

-- | @DELETE /Users/:userId/FavoriteItems/:itemId@ — clear favorite.
unfavoriteHandler :: [(Text, Text)] -> Request -> AppM Response
unfavoriteHandler captures req = do
  (uid, iid) <- captureUserItem captures
  _ <- requireUserDataAccess req uid
  _ <- requireExistingItem iid
  updateUserData uid iid (setFavorite False) >>= pure . jsonResponse

-- Helpers -------------------------------------------------------------------

-- | Merge a partial JSON patch (a decoded object) into an existing record:
-- only the keys actually present in the patch override the stored values.
-- A present-but-null optional field clears it. Wrongly typed values are a
-- 'Left' (the caller turns that into a 400).
mergeUserDataPatch :: HashMap Text Value -> UserData -> Either String UserData
mergeUserDataPatch patch ud = do
  played     <- field "played"           (udPlayed ud)
  favorite   <- field "favorite"         (udFavorite ud)
  position   <- field "playbackPosition" (udPlaybackPosition ud)
  playCount  <- field "playCount"        (udPlayCount ud)
  lastPlayed <- field "lastPlayedAt"     (udLastPlayedAt ud)
  pure (UserData played favorite position playCount lastPlayed)
  where
    field :: FromJSON a => Text -> a -> Either String a
    field k def = case HM.lookup k patch of
      Nothing -> Right def
      Just v  -> parseEither parseJSON v

-- | The @(userId, itemId)@ path captures, or 400 when missing.
captureUserItem :: [(Text, Text)] -> AppM (Text, Text)
captureUserItem captures = do
  uid <- capture "userId" captures
  iid <- capture "itemId" captures
  pure (uid, iid)

capture :: Text -> [(Text, Text)] -> AppM Text
capture name captures =
  case lookup name captures of
    Nothing -> throwError (BadRequest ("missing " <> name <> " capture"))
    Just v  -> pure v

-- | Auth gate for these routes: the request must carry a valid token
-- ('parseTokenHeader' over the raw request headers, resolved via
-- 'MonadAuth'), and the path's @:userId@ must be the token user's own id or
-- the token user must be an admin. Anything else -> 401.
requireUserDataAccess :: Request -> Text -> AppM User
requireUserDataAccess req targetUserId =
  case parseTokenHeader [(original k, v) | (k, v) <- requestHeaders req] of
    Nothing -> throwError (Unauthorized "authentication required")
    Just tok -> do
      mUser <- resolveToken tok
      case mUser of
        Nothing -> throwError (Unauthorized "invalid or expired token")
        Just u
          | isAdmin u || userId u == targetUserId -> pure u
          | otherwise -> throwError (Unauthorized "self or admin required")

-- | The item must exist in the library; 404 otherwise.
requireExistingItem :: Text -> AppM MediaItem
requireExistingItem iid = do
  m <- getItem iid
  case m of
    Nothing -> throwError (NotFound ("no library item with id " <> iid))
    Just item -> pure item
