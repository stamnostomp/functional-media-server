module Main (main) where

import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import FMS.Core.Types.Config (Config(..))
import FMS.Server (runServer)

-- | Entry point: configuration comes from env vars (with defaults), then the
-- server runs (docs/roadmap.org milestone 1).
main :: IO ()
main = do
  mPort  <- lookupEnv "MEDIA_SERVER_PORT"
  mLib   <- lookupEnv "MEDIA_LIBRARY_PATH"
  mState <- lookupEnv "FMS_STATE_DIR"
  let cfg = Config
        { configLibraryPath = fromMaybe "./media" mLib
        , configPort        = fromMaybe 8080 (mPort >>= readMaybe)
        , configStateDir    = fromMaybe "./var" mState
        }
  runServer cfg
