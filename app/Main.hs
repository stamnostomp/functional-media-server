module Main (main) where

import Control.Applicative ((<|>))
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.IO (BufferMode (..), hPutStrLn, hSetBuffering, stderr, stdout)
import Text.Read (readMaybe)

import FMS (name, version)
import FMS.Core.Cli (cliUsage, parseCliArgs)
import FMS.Core.Router (parseCorsOrigins)
import FMS.Core.Types.Config (Config (..), normalizeHwAccel, validateConfig)
import FMS.Core.Types.ServerConfig
  ( ServerConfig (..), loadServerConfig, serverConfigFilePath )
import FMS.Server (runServer)

-- | Entry point (docs/roadmap.org milestone 1, milestone 1b, milestone
-- 1d CLI args): configuration precedence is CLI flags > env vars >
-- @stateDir/config.json@ > defaults. @--help@\/@--version@ short-circuit
-- everything; an invalid config prints @config error: ...@ to stderr and
-- exits with code 2.
--
-- Env vars: MEDIA_SERVER_PORT, MEDIA_LIBRARY_PATH, FMS_STATE_DIR,
-- FMS_REQUIRE_AUTH, FMS_SERVER_NAME, MEDIA_TMDB_API_KEY, FMS_RATE_LIMIT,
-- FMS_LOG_LEVEL, FMS_CORS_ORIGINS, FMS_SECURITY_HEADERS,
-- FMS_PASSWORD_MIN_LENGTH, FMS_MAX_BITRATE, FMS_HWACCEL, FMS_WATCH,
-- FMS_FOLLOW_SYMLINKS. FMS_ADMIN_PASSWORD is read by the auth module
-- itself when it bootstraps the admin user.
main :: IO ()
main = do
  -- Line-buffer the process output so [INFO]/[ERROR] log lines show up in
  -- redirected log files immediately (milestone 2b: circular-symlink
  -- skips must be visible in the server log).
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  args <- getArgs
  cli <- case parseCliArgs args of
    Left err -> do
      hPutStrLn stderr ("argument error: " ++ T.unpack err)
      exitWith (ExitFailure 2)
    Right kvs -> pure kvs
  -- --help / --version take precedence over everything (milestone 1d)
  if lookup "help" cli == Just ""
    then putStr cliUsage >> exitSuccess
    else pure ()
  if lookup "version" cli == Just ""
    then putStrLn (name ++ " " ++ version) >> exitSuccess
    else pure ()
  -- CLI values (highest precedence), then env, then config.json
  let cliV k = lookup k cli
      cliBool k = case cliV k of
        Just v  -> Just (map toLower v `elem` ["true", "1", "yes"])
        Nothing -> Nothing
      cliInt k = cliV k >>= readMaybe
  mState <- lookupEnv "FMS_STATE_DIR"
  let stateDir = fromMaybe (fromMaybe "./var" mState) (cliV "state-dir")
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
  mMaxTr <- lookupEnv "FMS_MAX_TRANSCODES"
  mFfTo  <- lookupEnv "FMS_FFMPEG_TIMEOUT"
  mTmdb  <- lookupEnv "MEDIA_TMDB_API_KEY"
  let -- CLI > env for the value-style options
      portV   = cliInt "port" <|> (mPort >>= readMaybe)
      libV    = cliV "library-path" <|> mLib
      nameV   = cliV "server-name" <|> mName
      tmdbV   = cliV "tmdb-api-key" <|> mTmdb
      rateV   = cliV "rate-limit" <|> mRate
      logV    = cliV "log-level" <|> mLogLvl
      corsV   = cliV "cors-origins" <|> mCors
      pwMinV  = cliV "password-min-length" <|> mPwMin
      maxBrV  = cliV "max-bitrate" <|> mMaxBr
      hwAccV  = cliV "hwaccel" <|> mHwAcc
      -- booleans: CLI (true/false) > env > file/default
      boolV cliK mEnv def = fromMaybe (fromMaybe def (envBool mEnv)) (cliBool cliK)
      envBool (Just v) = Just (map toLower v `elem` ["true", "1", "yes"])
      envBool Nothing  = Nothing
      sc = fileCfg
        { scPort        = fromMaybe (scPort fileCfg) portV
        , scLibraryPath = fromMaybe (scLibraryPath fileCfg) libV
        , scRequireAuth = boolV "require-auth" mAuth (scRequireAuth fileCfg)
        , scServerName  = T.pack (fromMaybe (T.unpack (scServerName fileCfg)) nameV)
        }
      cfg0 = Config
        { configLibraryPath = scLibraryPath sc
        , configPort        = scPort sc
        , configStateDir    = stateDir
        , configRequireAuth = scRequireAuth sc
        , configServerName  = scServerName sc
        , configFollowSymlinks = boolV "follow-symlinks" mFollow False
        , configTmdbApiKey  = T.pack (fromMaybe "" tmdbV)
        , configRateLimit   = case rateV >>= readMaybe of
            Just n | n > 0 -> Just n
            _              -> Nothing   -- missing/invalid/0 -> disabled (milestone 14a)
        , configLogLevel    = T.pack (fromMaybe "info" logV)  -- milestone 11b; env-only
        , configCorsOrigins = maybe ["*"] parseCorsOrigins (T.pack <$> corsV)  -- milestone 13b
        , configSecurityHeaders = fromMaybe (fromMaybe True (envBool mSecH))
            (cliBool "security-headers")  -- --no-security-headers maps to "false" (milestone 13b)
        , configPasswordMinLength = case pwMinV >>= readMaybe of
            Just n | n > 0 -> n
            _              -> 0   -- missing/invalid/negative -> disabled (milestone 8b)
        , configMaxBitrate = case maxBrV >>= readMaybe of
            Just n | n > 0 -> n
            _              -> 0   -- missing/invalid/0 -> unlimited (milestone 6f)
        , configHwAccel = maybe "none" (normalizeHwAccel . T.pack) hwAccV  -- milestone 6f
        , configWatch = boolV "watch" mWatch False  -- default off (milestone 2d)
        , configMaxTranscodes = case mMaxTr >>= readMaybe of
            Just n | n > 0 -> n
            _              -> 2  -- missing/invalid/<=0 -> default 2 (milestone 15)
        , configFfmpegTimeoutSec = case mFfTo >>= readMaybe of
            Just n | n > 0 -> n
            _              -> 30 -- missing/invalid/<=0 -> default 30s (milestone 15)
        }
  cfg <- case validateConfig cfg0 of
    Left err -> do
      hPutStrLn stderr ("config error: " ++ T.unpack err)
      exitWith (ExitFailure 2)
    Right c -> pure c
  runServer cfg
