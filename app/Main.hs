module Main (main) where

import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

import FMS.Core.Types.Config (Config(..))
import FMS.Core.Types.ServerConfig
  ( ServerConfig(..), loadServerConfig, serverConfigFilePath )
import FMS.Server (runServer)

-- | Entry point (docs/roadmap.org milestone 1, milestone 1b): the server
-- configuration is loaded from @stateDir/config.json@ (missing file ->
-- defaults), then the env vars override the file:
-- MEDIA_SERVER_PORT, MEDIA_LIBRARY_PATH, FMS_REQUIRE_AUTH, FMS_SERVER_NAME
-- (and MEDIA_TMDB_API_KEY for the remote metadata fallback, milestone 4c).
-- FMS_ADMIN_PASSWORD is read by the auth module itself when it bootstraps
-- the admin user.
main :: IO ()
main = do
  mState <- lookupEnv "FMS_STATE_DIR"
  let stateDir = fromMaybe "./var" mState
  fileCfg <- loadServerConfig (serverConfigFilePath stateDir)
  mPort  <- lookupEnv "MEDIA_SERVER_PORT"
  mLib   <- lookupEnv "MEDIA_LIBRARY_PATH"
  mAuth  <- lookupEnv "FMS_REQUIRE_AUTH"
  mName  <- lookupEnv "FMS_SERVER_NAME"
  mTmdb  <- lookupEnv "MEDIA_TMDB_API_KEY"
  let sc = fileCfg
        { scPort        = fromMaybe (scPort fileCfg) (mPort >>= readMaybe)
        , scLibraryPath = fromMaybe (scLibraryPath fileCfg) mLib
        , scRequireAuth = case mAuth of
            Just a  -> map toLower a `elem` ["true", "1", "yes"]
            Nothing -> scRequireAuth fileCfg
        , scServerName  = T.pack (fromMaybe (T.unpack (scServerName fileCfg)) mName)
        }
      cfg = Config
        { configLibraryPath = scLibraryPath sc
        , configPort        = scPort sc
        , configStateDir    = stateDir
        , configRequireAuth = scRequireAuth sc
        , configServerName  = scServerName sc
        , configTmdbApiKey  = T.pack (fromMaybe "" mTmdb)
        }
  runServer cfg
