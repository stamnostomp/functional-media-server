module FMS.Server
  ( runServer
  , modules
  ) where

import Control.Concurrent
  ( MVar, forkIO, killThread, newEmptyMVar, takeMVar, threadDelay )
import Control.Concurrent.QSemN (newQSemN)
import Control.Concurrent.STM (newTVarIO)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Text (Text)
import Data.Time.Clock (getCurrentTime)
import Data.Acid (AcidState)
import Data.Acid.Local (createCheckpointAndClose)
import Data.Maybe (isJust)
import Network.Wai (Application, pathInfo)
import Network.WebSockets (defaultConnectionOptions)
import qualified Network.Wai.Handler.WebSockets as WS
import Network.Wai.Handler.Warp
  ( defaultSettings, runSettings, setGracefulShutdownTimeout, setPort )
import qualified Network.Wai.Handler.WarpTLS as WarpTLS
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs, getEnvironment, getExecutablePath)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)
import System.Process (CreateProcess (..), createProcess, proc)

import FMS (name, version)
import FMS.Core.Module (Module(..))
import FMS.Core.Monad (AppError, AppM, getState, newAppEnv, putState, runApp)
import FMS.Core.Process (ffmpegSlotsKey)
import FMS.Core.RateLimit (Bucket, rateLimitBucketsKey)
import FMS.Core.Router (buildApplication)
import FMS.Core.Types.Config (Config(..))
import FMS.Core.Types.Log (LogEntry(..))
import FMS.Core.Types.ServerConfig (ServerConfig (..), loadServerConfig, serverConfigFilePath)
import FMS.Modules.Admin (adminModule, MonadAdmin (..))
import FMS.Modules.ApiDocs (apiDocsModule)
import FMS.Modules.Auth (authModule)
import FMS.Modules.Collections (collectionsModule)
import FMS.Modules.Devices (devicesModule)
import FMS.Modules.Images (imagesModule)
import FMS.Modules.Library (libraryModule)
import FMS.Modules.Library.State (LibraryState)
import FMS.Modules.Metadata (metadataModule)
import FMS.Modules.Playback (playbackModule)
import FMS.Modules.Playlists (playlistsModule)
import FMS.Modules.Sessions (sessionsModule)
import FMS.Modules.Segments (segmentsModule)
import FMS.Modules.Socket (socketFallback, socketModule, socketServerApp)
import FMS.Modules.Dash (dashModule)
import FMS.Modules.Hls (hlsModule)
import FMS.Modules.Stats (statsModule)
import FMS.Modules.UserData (userDataModule)
import FMS.Modules.Webhooks (webhooksModule)
import FMS.Modules.System (systemModule)
import FMS.Modules.Stream (streamModule)
import FMS.Modules.Subtitles (subtitlesModule)
import FMS.Modules.Chapters (chaptersModule)
import FMS.Modules.Thumbs (thumbsModule)
import FMS.Modules.Transcode (transcodeModule)
import FMS.Modules.Web (webModule)

-- | The active module list. Adding a module never changes the core
-- (docs/modules.org).
modules :: [Module]
modules =
  [ systemModule, authModule, libraryModule, playbackModule, streamModule
  , transcodeModule
  -- dashModule BEFORE hlsModule: videos/:id/dash.mpd must not be captured
  -- by the HLS [videos, id, Capture seg] route (milestone 3g).
  , dashModule
  , hlsModule, subtitlesModule, chaptersModule, thumbsModule, imagesModule
  , statsModule
  , metadataModule, adminModule, userDataModule, sessionsModule, playlistsModule
  , collectionsModule, segmentsModule, socketModule
  , devicesModule
  , webhooksModule
  , webModule, apiDocsModule
  ]

-- | Boot the server: build the environment, register the runtime state
-- the core endpoints need (milestone 1b: the live 'ServerConfig' TVar,
-- the module-name list, the shutdown 'MVar'), run each module's init,
-- then serve HTTP with warp in a forked thread while the main thread
-- blocks on the shutdown MVar. When @POST /System/Shutdown@ or
-- @POST /System/Restart@ fires the MVar, warp is killed (its graceful
-- shutdown drains in-flight requests, 'setGracefulShutdownTimeout'), the
-- library acid-state is checkpointed, and the process exits -- or, for
-- "restart", re-execs itself with the same args and environment.
runServer :: Config -> IO ()
runServer cfg = do
  createDirectoryIfMissing True (configStateDir cfg)
  appEnv <- newAppEnv cfg
  -- Milestone 10b: the TVar starts from the persisted config.json (so
  -- StartupCompleted/LoginDisclaimer/CustomCss survive restarts), with the
  -- env-driven fields (already merged in app/Main) winning.
  fileSc <- loadServerConfig (serverConfigFilePath (configStateDir cfg))
  let sc = fileSc
        { scServerName  = configServerName cfg
        , scRequireAuth = configRequireAuth cfg
        , scLibraryPath = configLibraryPath cfg
        , scPort        = configPort cfg
        }
  scTv <- newTVarIO sc
  shutdownMv <- newEmptyMVar :: IO (MVar Text)
  _ <- runApp appEnv $ do
    putState "system.serverConfig" scTv
    -- Milestone 10d: process start time, read by GET /Dashboard for the
    -- UptimeSeconds field.
    startedAt <- liftIO getCurrentTime
    putState "system.startedAt" startedAt
    putState "system.moduleNames" (map moduleName modules)
    putState "system.shutdown" shutdownMv
    -- Milestone 11b: the server log ring buffer (read by
    -- GET /System/Logs/Log under "system.logbuf"; also created lazily by
    -- the MonadLog instance).
    logTv <- liftIO (newTVarIO ([] :: [LogEntry]))
    putState "system.logbuf" logTv
    -- Milestone 14a: the per-IP token-bucket map for rate limiting
    -- (read by the router under "ratelimit.buckets").
    rlTv <- liftIO (newTVarIO (HM.empty :: HM.HashMap Text Bucket))
    putState rateLimitBucketsKey rlTv
    -- Milestone 15: the global ffmpeg slot semaphore bounding concurrent
    -- transcode / HLS / DASH jobs (read via acquireFfmpegSlot under
    -- "ffmpeg.slots").
    ffSem <- liftIO (newQSemN (max 1 (configMaxTranscodes cfg)))
    putState ffmpegSlotsKey ffSem
    -- Milestone 1c: the module list itself, so the api-docs module can
    -- generate the OpenAPI spec from the actual route table.
    putState "system.modules" modules
  forM_ modules $ \m -> do
    r <- runApp appEnv (moduleInit m)
    case r of
      Left err -> hPutStrLn stderr
        ("[ERROR] module init failed: " ++ T.unpack (moduleName m) ++ ": " ++ show (err :: AppError))
      Right () -> putStrLn ("module loaded: " ++ T.unpack (moduleName m))
  -- Milestone 13d: when FMS_TLS_CERT + FMS_TLS_KEY are set, the same app
  -- (including the /Socket WebSocket upgrade -> wss) is served over TLS
  -- via warp-tls; otherwise plain HTTP as before.
  let tlsOn = isJust (configTlsCert cfg) && isJust (configTlsKey cfg)
  putStrLn (name ++ " " ++ version ++ " listening on port " ++ show (configPort cfg)
           ++ (if tlsOn then " (https)" else ""))
  let settings = setPort (configPort cfg)
                 $ setGracefulShutdownTimeout (Just 5)
                 $ defaultSettings
      routerApp = buildApplication appEnv modules
      -- Milestone 8: the WebSocket endpoint at /Socket (case-insensitive)
      -- is served via wai-websockets; everything else goes to the normal
      -- router app. A non-upgrade request to /Socket gets a plain 400.
      wsApp = WS.websocketsOr defaultConnectionOptions
                (socketServerApp appEnv) socketFallback
      app :: Application
      app req respond
        | map T.toLower (pathInfo req) == ["socket"] = wsApp req respond
        | otherwise = routerApp req respond
  warpThread <- forkIO $ case (configTlsCert cfg, configTlsKey cfg) of
    (Just cert, Just key) ->
      WarpTLS.runTLS (WarpTLS.tlsSettings cert key) settings app
    _ -> runSettings settings app
  tag <- takeMVar shutdownMv
  _ <- runApp appEnv (logActivity "Server" ("server " <> tag <> " requested"))
  putStrLn ("[INFO] " ++ T.unpack tag ++ " requested: shutting down gracefully")
  killThread warpThread   -- closes the listen socket, drains requests (<= 5s)
  threadDelay 500000      -- let warp release the port before a restart re-binds
  -- Persist the library acid-state so item ids survive the stop/restart.
  r <- runApp appEnv $ do
    mAcid <- getState "library.acid" :: AppM (Maybe (AcidState LibraryState))
    case mAcid of
      Just acid -> liftIO (createCheckpointAndClose acid)
      Nothing   -> pure ()
  case r of
    Left err -> hPutStrLn stderr ("[ERROR] acid checkpoint failed: " ++ show (err :: AppError))
    Right () -> pure ()
  case tag of
    "restart" -> do
      exe <- getExecutablePath
      args <- getArgs
      envVars <- getEnvironment
      putStrLn ("[INFO] restarting: " ++ exe ++ " (" ++ show (length envVars)
                ++ " env vars inherited)")
      -- Spawn the new process synchronously (createProcess fork+execs
      -- before returning), then exit: the child is orphaned to init and
      -- keeps serving while this process is gone immediately. (A blocking
      -- callProcess here would keep the old process alive as the child's
      -- parent, so spawn-without-wait is the right primitive.)
      _ <- createProcess (proc exe args) { env = Just envVars }
      exitWith ExitSuccess
    _ -> do
      putStrLn "[INFO] bye"
      exitWith ExitSuccess
