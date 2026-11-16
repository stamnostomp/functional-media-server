module Main (main) where

import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import FMS.Core.Types.Config (Config(..))
import FMS.Server (runServer)

-- | Entry point: configuration comes from env vars (with defaults), then the
-- server runs (docs/roadmap.org milestone 1). Milestone 5b adds
-- FMS_REQUIRE_AUTH (strict auth gate in the router); FMS_ADMIN_PASSWORD is
-- read by the auth module itself when it bootstraps the admin user.
main :: IO ()
main = do
  mPort  <- lookupEnv "MEDIA_SERVER_PORT"
  mLib   <- lookupEnv "MEDIA_LIBRARY_PATH"
  mState <- lookupEnv "FMS_STATE_DIR"
  mAuth  <- lookupEnv "FMS_REQUIRE_AUTH"
  let cfg = Config
        { configLibraryPath = fromMaybe "./media" mLib
        , configPort        = fromMaybe 8080 (mPort >>= readMaybe)
        , configStateDir    = fromMaybe "./var" mState
        , configRequireAuth = map toLower (fromMaybe "false" mAuth) `elem` ["true", "1", "yes"]
        }
  runServer cfg
