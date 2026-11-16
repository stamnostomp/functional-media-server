module FMS.Server
  ( runServer
  , modules
  ) where

import Control.Concurrent
  ( MVar, forkIO, killThread, newEmptyMVar, takeMVar, threadDelay )
import Control.Concurrent.STM (newTVarIO)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Acid (AcidState)
import Data.Acid.Local (createCheckpointAndClose)
import Network.Wai.Handler.Warp
  ( defaultSettings, runSettings, setGracefulShutdownTimeout, setPort )
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs, getEnvironment, getExecutablePath)
import System.Exit (ExitCode (..), exitWith)
import System.IO (hPutStrLn, stderr)
import System.Process (CreateProcess (..), createProcess, proc)

import FMS (name, version)
import FMS.Core.Module (Module(..))
import FMS.Core.Monad (AppError, AppM, getState, newAppEnv, putState, runApp)
import FMS.Core.Router (buildApplication)
import FMS.Core.Types.Config (Config(..))
import FMS.Core.Types.ServerConfig (ServerConfig (..))
import FMS.Modules.Admin (adminModule, MonadAdmin (..))
import FMS.Modules.Auth (authModule)
import FMS.Modules.Images (imagesModule)
import FMS.Modules.Library (libraryModule)
import FMS.Modules.Library.State (LibraryState)
import FMS.Modules.Metadata (metadataModule)
import FMS.Modules.Playback (playbackModule)
import FMS.Modules.Playlists (playlistsModule)
import FMS.Modules.Sessions (sessionsModule)
import FMS.Modules.Hls (hlsModule)
import FMS.Modules.Stats (statsModule)
import FMS.Modules.UserData (userDataModule)
import FMS.Modules.System (systemModule)
import FMS.Modules.Stream (streamModule)
import FMS.Modules.Subtitles (subtitlesModule)
import FMS.Modules.Thumbs (thumbsModule)
import FMS.Modules.Transcode (transcodeModule)

-- | The active module list. Adding a module never changes the core
-- (docs/modules.org).
modules :: [Module]
modules =
  [ systemModule, authModule, libraryModule, playbackModule, streamModule
  , transcodeModule
  , hlsModule, subtitlesModule, thumbsModule, imagesModule, statsModule
  , metadataModule, adminModule, userDataModule, sessionsModule, playlistsModule
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
  let sc = ServerConfig
        { scServerName  = configServerName cfg
        , scRequireAuth = configRequireAuth cfg
        , scLibraryPath = configLibraryPath cfg
        , scPort        = configPort cfg
        }
  scTv <- newTVarIO sc
  shutdownMv <- newEmptyMVar :: IO (MVar Text)
  _ <- runApp appEnv $ do
    putState "system.serverConfig" scTv
    putState "system.moduleNames" (map moduleName modules)
    putState "system.shutdown" shutdownMv
  forM_ modules $ \m -> do
    r <- runApp appEnv (moduleInit m)
    case r of
      Left err -> hPutStrLn stderr
        ("[ERROR] module init failed: " ++ T.unpack (moduleName m) ++ ": " ++ show (err :: AppError))
      Right () -> putStrLn ("module loaded: " ++ T.unpack (moduleName m))
  putStrLn (name ++ " " ++ version ++ " listening on port " ++ show (configPort cfg))
  let settings = setPort (configPort cfg)
                 $ setGracefulShutdownTimeout (Just 5)
                 $ defaultSettings
      app = buildApplication appEnv modules
  warpThread <- forkIO (runSettings settings app)
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
