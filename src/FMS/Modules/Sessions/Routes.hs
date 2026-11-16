module FMS.Modules.Sessions.Routes
  ( sessionsRoutes
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), eitherDecode, object, withObject, (.:), (.:?), (.=))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.Wai (Request, Response, strictRequestBody)

import FMS.Core.Module (Route, Segment (..), get, jsonResponse, post)
import FMS.Core.Monad (AppError (..), AppM)
import FMS.Core.Router (tokenFromRequest)
import FMS.Core.Types.User (User (..))
import FMS.Modules.Auth.Monad (MonadAuth (resolveToken))
import FMS.Modules.Library (MonadLibrary (..))
import FMS.Modules.Sessions.Monad (MonadSessions (..))

-- | The live playback session routes (docs/todo.org section 6, milestone
-- 6c), Jellyfin-shaped: @POST /Sessions/Playing@ starts a session,
-- @POST /Sessions/Playing/Progress@ and @.../Ping@ update it,
-- @POST /Sessions/Playing/Stopped@ ends it, and @GET /Sessions@ lists the
-- active ones. When the request carries a valid token
-- ('FMS.Core.Router.tokenFromRequest' + 'resolveToken') the session is
-- attached to that user (UserId/UserName); without a token the session is
-- anonymous. In strict auth mode the router gate already applies to these
-- paths; in open mode they are public.
sessionsRoutes :: [Route]
sessionsRoutes =
  [ post [Lit "Sessions", Lit "Playing"]                    playingHandler
  , post [Lit "Sessions", Lit "Playing", Lit "Progress"]     progressReportHandler
  , post [Lit "Sessions", Lit "Playing", Lit "Ping"]         pingHandler
  , post [Lit "Sessions", Lit "Playing", Lit "Stopped"]      stoppedHandler
  , get  [Lit "Sessions"]                                     listSessionsHandler
  ]

-- Request bodies ------------------------------------------------------------

-- | Body of @POST /Sessions/Playing@: @{\"ItemId\": \"1\", \"PlaySessionId\"?:
-- \"...\", \"PositionSeconds\"?: 0}@. The server mints the session id, so an
-- optional client @PlaySessionId@ is accepted in the body but the
-- canonical id is the minted one.
data PlayingRequest = PlayingRequest
  { plItemId          :: Text
  , plPositionSeconds :: Maybe Double
  }

instance FromJSON PlayingRequest where
  parseJSON = withObject "PlayingRequest" $ \o ->
    PlayingRequest
      <$> o .:  "ItemId"
      <*> o .:? "PositionSeconds"

-- | Body of @POST /Sessions/Playing/Progress@:
-- @{\"PlaySessionId\": \"...\", \"PositionSeconds\": 12.5, \"IsPaused\"?: false}@.
data ProgressReport = ProgressReport
  { prSessionId       :: Text
  , prPositionSeconds :: Double
  , prIsPaused        :: Maybe Bool
  }

instance FromJSON ProgressReport where
  parseJSON = withObject "ProgressReport" $ \o ->
    ProgressReport
      <$> o .:  "PlaySessionId"
      <*> o .:  "PositionSeconds"
      <*> o .:? "IsPaused"

-- | Body of @POST /Sessions/Playing/Ping@: @{\"PlaySessionId\": \"...\"}@.
data PingRequest = PingRequest
  { pgSessionId :: Text
  }

instance FromJSON PingRequest where
  parseJSON = withObject "PingRequest" $ \o ->
    PingRequest <$> o .: "PlaySessionId"

-- | Body of @POST /Sessions/Playing/Stopped@:
-- @{\"ItemId\": \"1\", \"PlaySessionId\": \"...\", \"PositionSeconds\"?: 30}@.
data StoppedRequest = StoppedRequest
  { stItemId          :: Text
  , stSessionId       :: Text
  , stPositionSeconds :: Maybe Double
  }

instance FromJSON StoppedRequest where
  parseJSON = withObject "StoppedRequest" $ \o ->
    StoppedRequest
      <$> o .:  "ItemId"
      <*> o .:  "PlaySessionId"
      <*> o .:? "PositionSeconds"

-- Handlers ------------------------------------------------------------------

-- | @POST /Sessions/Playing@ — start a session for @ItemId@. 404 when the
-- item is not in the library, 400 on a malformed body, otherwise 200 with
-- the new session's JSON (Id, ItemId, UserId/UserName when a valid token
-- was sent, PositionSeconds, IsPaused, StartedAt, LastPingAt).
playingHandler :: [(Text, Text)] -> Request -> AppM Response
playingHandler _captures req = do
  body <- liftIO (strictRequestBody req)
  case eitherDecode body of
    Left err -> throwError (BadRequest ("invalid JSON body: " <> T.pack err))
    Right pr -> do
      mItem <- getItem (plItemId pr)
      case mItem of
        Nothing -> throwError (NotFound ("no library item with id " <> plItemId pr))
        Just _ -> do
          mUser <- currentUserFromRequest req
          let pos = fromMaybe 0 (plPositionSeconds pr)
          s <- startSession (plItemId pr) (userId <$> mUser) pos
          pure (jsonResponse s)

-- | @POST /Sessions/Playing/Progress@ — update position/pause of a live
-- session. 400 on a malformed body, 404 when the session id is unknown,
-- otherwise 200 with the updated session JSON.
progressReportHandler :: [(Text, Text)] -> Request -> AppM Response
progressReportHandler _captures req = do
  body <- liftIO (strictRequestBody req)
  case eitherDecode body of
    Left err -> throwError (BadRequest ("invalid JSON body: " <> T.pack err))
    Right pr -> do
      mS <- progressSession (prSessionId pr) (prPositionSeconds pr)
                 (fromMaybe False (prIsPaused pr))
      case mS of
        Nothing -> throwError (NotFound ("no active session with id " <> prSessionId pr))
        Just s  -> pure (jsonResponse s)

-- | @POST /Sessions/Playing/Ping@ — heartbeat: bumps LastPingAt so the
-- session is not expired as stale. 400 on a malformed body, 404 for an
-- unknown session id, otherwise 200 with the session JSON.
pingHandler :: [(Text, Text)] -> Request -> AppM Response
pingHandler _captures req = do
  body <- liftIO (strictRequestBody req)
  case eitherDecode body of
    Left err -> throwError (BadRequest ("invalid JSON body: " <> T.pack err))
    Right pr -> do
      mS <- pingSession (pgSessionId pr)
      case mS of
        Nothing -> throwError (NotFound ("no active session with id " <> pgSessionId pr))
        Just s  -> pure (jsonResponse s)

-- | @POST /Sessions/Playing/Stopped@ — end a session (removed from the
-- active map). 400 on a malformed body, 404 when the item is unknown;
-- otherwise 200 @{\"status\":\"ok\"}@ (removing an already-ended session
-- is a no-op). The final position is tracked with the session only —
-- stats stay with the stats module (plays are recorded on /stream).
stoppedHandler :: [(Text, Text)] -> Request -> AppM Response
stoppedHandler _captures req = do
  body <- liftIO (strictRequestBody req)
  case eitherDecode body of
    Left err -> throwError (BadRequest ("invalid JSON body: " <> T.pack err))
    Right sr -> do
      mItem <- getItem (stItemId sr)
      case mItem of
        Nothing -> throwError (NotFound ("no library item with id " <> stItemId sr))
        Just _ -> do
          stopSession (stSessionId sr) (stPositionSeconds sr)
          pure (jsonResponse (object ["status" .= ("ok" :: Text)]))

-- | @GET /Sessions@ — 200 JSON array of the active sessions (stale ones
-- expired first). Public in open mode; the router's auth gate applies in
-- strict mode.
listSessionsHandler :: [(Text, Text)] -> Request -> AppM Response
listSessionsHandler _captures _req =
  jsonResponse <$> listSessions

-- Helpers -------------------------------------------------------------------

-- | The user behind the request's credential, if any: parse the token
-- header and resolve it (session token or API key) via the auth module.
currentUserFromRequest :: Request -> AppM (Maybe User)
currentUserFromRequest req =
  case tokenFromRequest req of
    Nothing  -> pure Nothing
    Just tok -> resolveToken tok
