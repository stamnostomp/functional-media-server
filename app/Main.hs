module Main (main) where

import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import System.IO (BufferMode (..), hSetBuffering, stderr, stdout)
import Text.Read (readMaybe)

import FMS.Core.Router (parseCorsOrigins)
import FMS.Core.Types.Config (Config(..))
import FMS.Modules.Transcode (normalizeHwAccel)
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
  -- Line-buffer the process output so [INFO]/[ERROR] log lines show up in
  -- redirected log files immediately (milestone 2b: circular-symlink
  -- skips must be visible in the server log).
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  mState <- lookupEnv "FMS_STATE_DIR"
  let stateDir = fromMaybe "./var" mState
  fileCfg <- loadServerConfig (serverConfigFilePath stateDir)
  mPort  <- lookupEnv "MEDIA_SERVER_PORT"
  mLib   <- lookupEnv "MEDIA_LIBRARY_PATH"
  mAuth  <- lookupEnv "FMS_REQUIRE_AUTH"
  mName  <- lookupEnv "FMS_SERVER_NAME"
  mFollow <- lookupEnv "FMS_FOLLOW_SYMLINKS"
  mRate  <- lookupEnv "FMS_RATE_LIMIT"
  mLogLvl <- lookupEnv "FMS_LOG_LEVEL"
  mCors  <- lookupEnv "FMS_CORS_ORIGINS"
  mSecH  <- lookupEnv "FMS_SECURITY_HEADERS"
  mPwMin <- lookupEnv "FMS_PASSWORD_MIN_LENGTH"
  mMaxBr <- lookupEnv "FMS_MAX_BITRATE"
  mHwAcc <- lookupEnv "FMS_HWACCEL"
  mWatch <- lookupEnv "FMS_WATCH"
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
        , configFollowSymlinks = maybe False
            (\v -> map toLower v `elem` ["true", "1", "yes"]) mFollow
        , configTmdbApiKey  = T.pack (fromMaybe "" mTmdb)
        , configRateLimit   = case mRate >>= readMaybe of
            Just n | n > 0 -> Just n
            _              -> Nothing   -- missing/invalid/0 -> disabled (milestone 14a)
        , configLogLevel    = T.pack (fromMaybe "info" mLogLvl)  -- milestone 11b; env-only
        , configCorsOrigins = maybe ["*"] parseCorsOrigins (T.pack <$> mCors)  -- milestone 13b
        , configSecurityHeaders = case mSecH of
            Just v -> map toLower v `elem` ["true", "1", "yes"]
            Nothing -> True   -- default on (milestone 13b)
        , configPasswordMinLength = case mPwMin >>= readMaybe of
            Just n | n > 0 -> n
            _              -> 0   -- missing/invalid/negative -> disabled (milestone 8b)
        , configMaxBitrate = case mMaxBr >>= readMaybe of
            Just n | n > 0 -> n
            _              -> 0   -- missing/invalid/0 -> unlimited (milestone 6f)
        , configHwAccel = maybe "none" (normalizeHwAccel . T.pack) mHwAcc  -- milestone 6f
        , configWatch = case mWatch of
            Just v -> map toLower v `elem` ["true", "1", "yes"]
            Nothing -> False   -- default off (milestone 2d)
        }
  runServer cfg
