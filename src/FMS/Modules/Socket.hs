module FMS.Modules.Socket
  ( socketModule
  , socketServerApp
  , socketFallback
  , socketConnsKey
  , encodeMessage
  , parseApiKey
  , parseApiKeyFromPath
  , addConn
  , removeConn
  , broadcastSocket
  , broadcastSocketToToken
  ) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Control.Exception (SomeException, try)
import Control.Monad (forM_, forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (Value, encode, object, (.=))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Unique (Unique, newUnique)
import Network.HTTP.Types (parseQuery, status400)
import Network.Wai (Application, responseLBS)
import Network.WebSockets
  ( Connection
  , ServerApp
  , acceptRequest
  , pendingRequest
  , receiveData
  , rejectRequest
  , requestPath
  , sendTextData
  )

import FMS.Core.Module (Module (..))
import FMS.Core.Monad (AppEnv, AppM, envConfig, getState, putState, runApp)
import FMS.Core.Query (queryParams)
import FMS.Core.Types.Config (Config (..))
import FMS.Modules.Auth.Monad (MonadAuth (..))

-- | The WebSocket module (milestone 8): the Jellyfin socket API at
-- @GET /Socket@. Auth is the @api_key@ (or @token@) query parameter,
-- resolved with the same 'resolveToken' the HTTP router uses; in open mode
-- (configRequireAuth = False) any key is accepted. Accepted connections
-- are registered in a TVar under 'socketConnsKey' so other modules can
-- broadcast server->client messages ('broadcastSocket'), and each
-- connection gets a keepalive thread sending @KeepAlive@ every 10s.
-- The route itself is intercepted in "FMS.Server" (path /Socket) and
-- served via @wai-websockets@, so this module registers no HTTP routes.
socketModule :: Module
socketModule = Module
  { moduleName   = "socket"
  , moduleInit   = do
      tv <- liftIO (newTVarIO ([] :: [(Unique, (Maybe Text, Connection))]))
      putState socketConnsKey tv
  , moduleRoutes = []
  }

-- | Namespaced registry key for the live connections:
-- @TVar [(Unique, (Maybe Text, Connection))]@ — each entry also carries
-- the credential (api_key/token) the socket authenticated with, so
-- targeted messages can reach one session's socket
-- ('broadcastSocketToToken'). Keyed because 'Connection' has no 'Eq'
-- instance, so removal needs a stable key.
socketConnsKey :: Text
socketConnsKey = "socket.conns"

-- | Pure: encode a server->client socket message as
-- @{\"MessageType\": t, \"Data\": d}@.
encodeMessage :: Text -> Value -> LBS.ByteString
encodeMessage t d = encode (object ["MessageType" .= t, "Data" .= d])

-- | Pure: extract the credential from the socket query string — the
-- @api_key@ param (Jellyfin convention) or @token@; empty values are
-- ignored.
parseApiKey :: [(Text, Text)] -> Maybe Text
parseApiKey qs =
  case lookup "api_key" qs of
    Just k | not (T.null k) -> Just k
    _ -> case lookup "token" qs of
      Just k | not (T.null k) -> Just k
      _                       -> Nothing

-- | Pure: extract the credential from the raw socket request path
-- (@\/Socket?api_key=...@). websockets-0.13's 'RequestHead' carries no
-- parsed query, so the query part of 'requestPath' is parsed with
-- http-types' 'parseQuery' and fed through 'parseApiKey'.
parseApiKeyFromPath :: B.ByteString -> Maybe Text
parseApiKeyFromPath path =
  parseApiKey (queryParams (parseQuery (B.drop 1 (snd (B.breakSubstring "?" path)))))

-- | Pure: register a connection in the registry (keyed append).
addConn :: Eq k => k -> a -> [(k, a)] -> [(k, a)]
addConn k c cs = cs ++ [(k, c)]

-- | Pure: unregister the connection with the given key.
removeConn :: Eq k => k -> [(k, a)] -> [(k, a)]
removeConn k = filter ((/= k) . fst)

-- | The @ServerApp@ for @\/Socket@ (served via
-- 'Network.Wai.Handler.WebSockets.websocketsApp' in "FMS.Server").
-- Validates @api_key@/token against the auth token registry (skipped in
-- open mode), rejects with 403 otherwise, then registers the connection,
-- starts the keepalive thread, and discards incoming client messages
-- until the connection closes.
socketServerApp :: AppEnv -> ServerApp
socketServerApp env pending = do
  -- 'pendingRequest' yields a websockets 'RequestHead' (not a WAI
  -- 'Request'); its 'requestPath' still carries the raw query string.
  let mKey = parseApiKeyFromPath (requestPath (pendingRequest pending))
  rok <- runApp env $ do
    cfg <- asks envConfig
    if configRequireAuth cfg
      then case mKey of
        Nothing -> pure False
        Just k  -> isJust <$> resolveToken k
      else pure True
  let ok = either (const False) id rok
  if not ok
    then rejectRequest pending "403 Forbidden"
    else do
      conn <- acceptRequest pending
      u <- newUnique
      _ <- runApp env (registerConn u mKey conn)
      keepAlive <- forkIO (keepAliveLoop conn)
      receiveLoop conn
      killThread keepAlive
      _ <- runApp env (unregisterConn u)
      pure ()
  where
    receiveLoop conn = do
      r <- try ((receiveData conn :: IO B.ByteString) >> pure ())
             :: IO (Either SomeException ())
      case r of
        Left _  -> pure ()   -- ConnectionClosed / client gone: stop looping
        Right _ -> receiveLoop conn

-- | Send @KeepAlive@ every 10 seconds; a dead client throws
-- 'ConnectionClosed', which is swallowed so the thread never crashes.
keepAliveLoop :: Connection -> IO ()
keepAliveLoop conn = forever $ do
  threadDelay 10000000
  _ <- try (sendTextData conn (encodeMessage "KeepAlive" (object [])))
         :: IO (Either SomeException ())
  pure ()

registerConn :: Unique -> Maybe Text -> Connection -> AppM ()
registerConn u mKey conn = do
  mTv <- getState socketConnsKey :: AppM (Maybe (TVar [(Unique, (Maybe Text, Connection))]))
  case mTv of
    Just tv -> liftIO (atomically (modifyTVar' tv (addConn u (mKey, conn))))
    Nothing -> pure ()

unregisterConn :: Unique -> AppM ()
unregisterConn u = do
  mTv <- getState socketConnsKey :: AppM (Maybe (TVar [(Unique, Connection)]))
  case mTv of
    Just tv -> liftIO (atomically (modifyTVar' tv (removeConn u)))
    Nothing -> pure ()

-- | Broadcast a server->client message to every live socket connection.
-- Failures (dead clients) are swallowed — broadcasting must never break
-- the caller.
broadcastSocket :: Text -> Value -> AppM ()
broadcastSocket msgType dat = do
  mTv <- getState socketConnsKey :: AppM (Maybe (TVar [(Unique, (Maybe Text, Connection))]))
  case mTv of
    Nothing -> pure ()
    Just tv -> do
      conns <- liftIO (map (snd . snd) <$> readTVarIO tv)
      forM_ conns $ \conn ->
        liftIO (void (try (sendTextData conn (encodeMessage msgType dat))
                        :: IO (Either SomeException ())))

-- | Broadcast a server->client message only to the socket connections
-- that authenticated with this token (i.e. one session's socket, when
-- connected). Used for per-session GeneralCommand delivery. Failures
-- are swallowed like in 'broadcastSocket'.
broadcastSocketToToken :: Text -> Text -> Value -> AppM ()
broadcastSocketToToken tok msgType dat = do
  mTv <- getState socketConnsKey :: AppM (Maybe (TVar [(Unique, (Maybe Text, Connection))]))
  case mTv of
    Nothing -> pure ()
    Just tv -> do
      conns <- liftIO (readTVarIO tv)
      forM_ [ c | (_, (mTok, c)) <- conns, mTok == Just tok ] $ \conn ->
        liftIO (void (try (sendTextData conn (encodeMessage msgType dat))
                        :: IO (Either SomeException ())))

-- | Fallback 'Application' for a non-WebSocket request to @\/Socket@:
-- plain 400 (used by "FMS.Server").
socketFallback :: Application
socketFallback _req respond =
  respond (responseLBS status400 [("Content-Type", "text/plain")]
           "expected a WebSocket upgrade on /Socket")
