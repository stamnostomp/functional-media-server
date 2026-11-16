module FMS.Server
  ( runServer
  , modules
  ) where

import Control.Monad (forM_)
import qualified Data.Text as T
import Network.Wai.Handler.Warp (run)
import System.Directory (createDirectoryIfMissing)
import System.IO (hPutStrLn, stderr)

import FMS (name, version)
import FMS.Core.Module (Module(..))
import FMS.Core.Monad (AppError, runApp, newAppEnv)
import FMS.Core.Router (buildApplication)
import FMS.Core.Types.Config (Config(..))
import FMS.Modules.Library (libraryModule)
import FMS.Modules.Hls (hlsModule)
import FMS.Modules.Stream (streamModule)
import FMS.Modules.Subtitles (subtitlesModule)
import FMS.Modules.Thumbs (thumbsModule)
import FMS.Modules.System (systemModule)
import FMS.Modules.Transcode (transcodeModule)

-- | The active module list. Adding a module never changes the core
-- (docs/modules.org).
modules :: [Module]
modules =
  [ systemModule, libraryModule, streamModule, transcodeModule, hlsModule
  , subtitlesModule, thumbsModule
  ]

-- | Boot the server: build the environment, run each module's init, then
-- serve HTTP with warp.
runServer :: Config -> IO ()
runServer cfg = do
  createDirectoryIfMissing True (configStateDir cfg)
  env <- newAppEnv cfg
  forM_ modules $ \m -> do
    r <- runApp env (moduleInit m)
    case r of
      Left err -> hPutStrLn stderr
        ("[ERROR] module init failed: " ++ T.unpack (moduleName m) ++ ": " ++ show (err :: AppError))
      Right () -> putStrLn ("module loaded: " ++ T.unpack (moduleName m))
  putStrLn (name ++ " " ++ version ++ " listening on port " ++ show (configPort cfg))
  run (configPort cfg) (buildApplication env modules)
