module FMS.Modules.Sessions.Routes
  ( sessionsRoutes
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as BL
import Data.Aeson (FromJSON (..), ToJSON (..), Value, eitherDecode, object, withObject, (.:), (.:?), (.=))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types (status204)
import Network.Wai (Request, Response, responseLBS, strictRequestBody)

import FMS.Core.Module (Route, Segment (..), get, jsonResponse, post)
import FMS.Core.Monad (AppError (..), AppM)
import FMS.Core.Router (tokenFromRequest)
import FMS.Core.Types.User (User (..))
import FMS.Modules.Auth.Monad (MonadAuth (resolveToken), currentUserFromReq)
import FMS.Modules.Socket (broadcastSocket, broadcastSocketToToken)
import FMS.Modules.Webhooks (fireWebhooks)
import FMS.Modules.Library (MonadLibrary (..))
import FMS.Modules.Sessions.Monad
  ( MonadSessions (..)
  , applySessionCapabilitiesTo
  , ensureSessionForToken
  , getSessionById
  , recordSessionCommand
  , setSessionToken
  )
import FMS.Modules.Sessions.Types
  ( PlaySession (..)
  , SessionCapabilities (..)
  , emptyCapabilities
  , validCommandName
  )

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
  , post [Lit "Sessions", Lit "Capabilities", Lit "Full"]    capabilitiesHandler
  , post [Lit "Sessions", Lit "Capabilities"]                capabilitiesHandler
  , get  [Lit "Sessions"]                                     listSessionsHandler
  -- capture routes come after the literal ones (first match wins)
  , get  [Lit "Sessions", Capture "id"]                       sessionViewHandler
  , post [Lit "Sessions", Capture "id", Lit "Command"]        commandHandler
  , post [Lit "Sessions", Capture "id", Lit "Command", Capture "command"]
                                                              commandPathHandler
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
          -- remember the creating token so capabilities/commands can
          -- find this session and its websocket later
          _ <- case tokenFromRequest req of
                 Just tok -> setSessionToken (psId s) tok
                 Nothing  -> pure Nothing
          _ <- broadcastSocket "PlaybackStart" (toJSON s)   -- milestone 8
          fireWebhooks "PlaybackStart" (toJSON s)           -- milestone 12
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
        Just s  -> do
          _ <- broadcastSocket "PlaybackProgress" (toJSON s)   -- milestone 8
          fireWebhooks "PlaybackProgress" (toJSON s)           -- milestone 12
          pure (jsonResponse s)

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
          _ <- broadcastSocket "PlaybackStop" (object   -- milestone 8
                 [ "ItemId"          .= stItemId sr
                 , "PlaySessionId"   .= stSessionId sr
                 , "PositionSeconds" .= fromMaybe 0 (stPositionSeconds sr)
                 ])
          fireWebhooks "PlaybackStop" (object   -- milestone 12
                 [ "ItemId"          .= stItemId sr
                 , "PlaySessionId"   .= stSessionId sr
                 , "PositionSeconds" .= fromMaybe 0 (stPositionSeconds sr)
                 ])
          pure (jsonResponse (object ["status" .= ("ok" :: Text)]))

-- | @GET /Sessions@ — 200 JSON array of the active sessions (stale ones
-- expired first). Public in open mode; the router's auth gate applies in
-- strict mode.
listSessionsHandler :: [(Text, Text)] -> Request -> AppM Response
listSessionsHandler _captures _req =
  jsonResponse <$> listSessions

-- | Body of @POST /Sessions/:id/Command@:
-- @{\"Name\": \"SetVolume\", \"Arguments\"?: {...}}@.
data CommandRequest = CommandRequest
  { crName      :: Text
  , crArguments :: Maybe Value
  }

instance FromJSON CommandRequest where
  parseJSON = withObject "CommandRequest" $ \o ->
    CommandRequest
      <$> o .:  "Name"
      <*> o .:? "Arguments"

-- | @POST /Sessions/Capabilities/Full@ (and the @POST
-- /Sessions/Capabilities@ alias) — 204. Body is a partial capability
-- object (@PlayableMediaTypes@, @SupportedCommands@,
-- @SupportsMediaControl@, @SupportsPersistentIdentifier@, @DeviceId@);
-- every field is optional and an EMPTY body is accepted (all defaults).
-- The capabilities are upserted onto the session identified by the
-- request's auth token, creating a lightweight session when none exists
-- yet. 400 on malformed JSON, 401 without a valid token.
capabilitiesHandler :: [(Text, Text)] -> Request -> AppM Response
capabilitiesHandler _captures req = do
  body <- liftIO (strictRequestBody req)
  caps <- if BL.null body
            then pure emptyCapabilities
            else case eitherDecode body of
                   Left err -> throwError (BadRequest ("invalid JSON body: " <> T.pack err))
                   Right c  -> pure (c :: SessionCapabilities)
  case tokenFromRequest req of
    Nothing  -> throwError (Unauthorized "authentication required")
    Just tok -> do
      mUser <- resolveToken tok
      s <- ensureSessionForToken tok (userId <$> mUser)
      _ <- applySessionCapabilitiesTo (psId s) caps
      pure (responseLBS status204 [] "")

-- | @GET /Sessions/:id@ — 200 with the session JSON (including the
-- capability fields when set). Admin or the session owner only:
-- 401 without a valid token, 403 for another user's session, 404 for an
-- unknown id.
sessionViewHandler :: [(Text, Text)] -> Request -> AppM Response
sessionViewHandler captures req = do
  mUser <- currentUserFromReq req
  case mUser of
    Nothing  -> throwError (Unauthorized "authentication required")
    Just u -> do
      let sid = fromMaybe "" (lookup "id" captures)
      mS <- getSessionById sid
      case mS of
        Nothing -> throwError (NotFound ("no active session with id " <> sid))
        Just s
          | isAdmin u                        -> pure (jsonResponse s)
          | psUserId s == Just (userId u)    -> pure (jsonResponse s)
          | otherwise                        -> throwError (Forbidden "not your session")

-- | @POST /Sessions/:id/Command@ — body @{\"Name\": ..., \"Arguments\"?:
-- ...}@; 204 on success. The command is broadcast as a websocket
-- @GeneralCommand@ to the session's socket (when connected) and recorded
-- as LastCommand on the session. 400 on a malformed body or an invalid
-- command name, 401/403 per the view rule, 404 for an unknown session.
commandHandler :: [(Text, Text)] -> Request -> AppM Response
commandHandler captures req = do
  body <- liftIO (strictRequestBody req)
  case eitherDecode body of
    Left err -> throwError (BadRequest ("invalid JSON body: " <> T.pack err))
    Right cr -> runCommand captures req (crName cr) (fromMaybe (object []) (crArguments cr))

-- | @POST /Sessions/:id/Command/:command@ — same as 'commandHandler' but
-- the command name comes from the path segment and no body is needed.
commandPathHandler :: [(Text, Text)] -> Request -> AppM Response
commandPathHandler captures req =
  runCommand captures req
    (fromMaybe "" (lookup "command" captures))
    (object [])

-- | Shared implementation of the two command routes: validate the name,
-- enforce admin-or-owner, record LastCommand, targeted-broadcast
-- GeneralCommand, then 204.
runCommand :: [(Text, Text)] -> Request -> Text -> Value -> AppM Response
runCommand captures req cmd args
  | not (validCommandName cmd) =
      throwError (BadRequest "invalid command name (empty, over 64 chars, or control chars)")
  | otherwise = do
      mUser <- currentUserFromReq req
      case mUser of
        Nothing -> throwError (Unauthorized "authentication required")
        Just u -> do
          let sid = fromMaybe "" (lookup "id" captures)
          mS <- getSessionById sid
          case mS of
            Nothing -> throwError (NotFound ("no active session with id " <> sid))
            Just s
              | not (isAdmin u) && psUserId s /= Just (userId u) ->
                  throwError (Forbidden "not your session")
              | otherwise -> do
                  _ <- recordSessionCommand sid cmd
                  case psToken s of
                    Just tok -> broadcastSocketToToken tok "GeneralCommand"
                                  (object [ "Name"      .= cmd
                                          , "Arguments" .= args
                                          , "SessionId" .= sid
                                          ])
                    Nothing  -> pure ()
                  pure (responseLBS status204 [] "")

-- Helpers -------------------------------------------------------------------

-- | The user behind the request's credential, if any: parse the token
-- header and resolve it (session token or API key) via the auth module.
currentUserFromRequest :: Request -> AppM (Maybe User)
currentUserFromRequest req =
  case tokenFromRequest req of
    Nothing  -> pure Nothing
    Just tok -> resolveToken tok
