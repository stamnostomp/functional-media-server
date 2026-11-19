module Main (main) where

import Control.Applicative ((<|>))
import Control.Monad (unless, when)
import Data.Char (toLower)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist, doesFileExist)
import System.Environment (getArgs, lookupEnv)
import System.Exit (ExitCode (..), exitSuccess, exitWith)
import System.IO (BufferMode (..), hPutStrLn, hSetBuffering, stderr, stdout)
import System.FilePath ((</>))
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
-- FMS_RATE_LIMIT_AUTH, FMS_SCAN_WORKERS, FMS_MAX_BODY_BYTES,
-- FMS_MEMORY_WARN_MB, FMS_MEMORY_SAMPLE_SECS (memory monitoring),
-- FMS_LOG_LEVEL, FMS_CORS_ORIGINS, FMS_SECURITY_HEADERS,
-- FMS_PASSWORD_MIN_LENGTH, FMS_MAX_BITRATE, FMS_HWACCEL, FMS_WATCH,
-- FMS_FOLLOW_SYMLINKS, FMS_TLS_CERT, FMS_TLS_KEY (milestone 13d),
-- FMS_FETCH_IMAGES, FMS_TMDB_BASE_URL, FMS_TMDB_IMAGE_BASE_URL,
-- FMS_TMDB_IMAGE_SIZE, FMS_TMDB_BACKDROP_SIZE (TMDB artwork download,
-- opt-in via FMS_FETCH_IMAGES), FMS_SERIES_CACHE_TTL_SECONDS (TMDB series
-- metadata cache TTL, default 86400, 0 = always refetch; m56).
-- FMS_ADMIN_PASSWORD is read by the auth module itself when it
-- bootstraps the admin user.
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
  mRateAuth <- lookupEnv "FMS_RATE_LIMIT_AUTH"
  mScanW <- lookupEnv "FMS_SCAN_WORKERS"
  mMaxBody <- lookupEnv "FMS_MAX_BODY_BYTES"
  mMemWarn <- lookupEnv "FMS_MEMORY_WARN_MB"
  mMemSamp <- lookupEnv "FMS_MEMORY_SAMPLE_SECS"
  mLogLvl <- lookupEnv "FMS_LOG_LEVEL"
  mCors  <- lookupEnv "FMS_CORS_ORIGINS"
  mSecH  <- lookupEnv "FMS_SECURITY_HEADERS"
  mPwMin <- lookupEnv "FMS_PASSWORD_MIN_LENGTH"
  mMaxBr <- lookupEnv "FMS_MAX_BITRATE"
  mHwAcc <- lookupEnv "FMS_HWACCEL"
  mWatch <- lookupEnv "FMS_WATCH"
  mMaxTr <- lookupEnv "FMS_MAX_TRANSCODES"
  mFfTo  <- lookupEnv "FMS_FFMPEG_TIMEOUT"
  mTlsC  <- lookupEnv "FMS_TLS_CERT"
  mTlsK  <- lookupEnv "FMS_TLS_KEY"
  mUiDir <- lookupEnv "FMS_UI_DIR"
  mTmdb  <- lookupEnv "MEDIA_TMDB_API_KEY"
  mFetchImgs <- lookupEnv "FMS_FETCH_IMAGES"
  mTmdbBase  <- lookupEnv "FMS_TMDB_BASE_URL"
  mTmdbImgBase <- lookupEnv "FMS_TMDB_IMAGE_BASE_URL"
  mTmdbImgSize <- lookupEnv "FMS_TMDB_IMAGE_SIZE"
  mTmdbBdSize  <- lookupEnv "FMS_TMDB_BACKDROP_SIZE"
  mSeriesCacheTtl <- lookupEnv "FMS_SERIES_CACHE_TTL_SECONDS"
  mJackettUrl <- lookupEnv "FMS_JACKETT_URL"
  mJackettKey <- lookupEnv "FMS_JACKETT_API_KEY"
  mDelugeUrl  <- lookupEnv "FMS_DELUGE_URL"
  mDelugePw   <- lookupEnv "FMS_DELUGE_PASSWORD"
  mDlDir      <- lookupEnv "FMS_DOWNLOAD_DIR"
  mAutoAppr   <- lookupEnv "FMS_REQUEST_AUTO_APPROVE_GB"
  mIndexerUrl <- lookupEnv "FMS_INDEXER_URL"
  mIndexerKey <- lookupEnv "FMS_INDEXER_API_KEY"
  mIndexerNm  <- lookupEnv "FMS_INDEXER_NAME"
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
      tlsCV   = cliV "tls-cert" <|> mTlsC
      tlsKV   = cliV "tls-key" <|> mTlsK
      uiDirV  = cliV "ui-dir" <|> mUiDir
      -- booleans: CLI (true/false) > env > file/default
      boolV cliK mEnv def = fromMaybe (fromMaybe def (envBool mEnv)) (cliBool cliK)
      envBool (Just v) = Just (map toLower v `elem` ["true", "1", "yes"])
      envBool Nothing  = Nothing
      sc = fileCfg
        { scPort        = fromMaybe (scPort fileCfg) portV
        , scLibraryPath = fromMaybe (scLibraryPath fileCfg) libV
        , scRequireAuth = boolV "require-auth" mAuth (scRequireAuth fileCfg)
        , scServerName  = T.pack (fromMaybe (T.unpack (scServerName fileCfg)) nameV)
        , scUiDir       = maybe (scUiDir fileCfg) Just uiDirV  -- CLI/env wins over config.json
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
        , configRateLimitAuth = case mRateAuth >>= readMaybe of
            Just n  -> Just n   -- negative rejected by validateConfig (exit 2)
            Nothing -> Just 10  -- unset -> default 10 auth attempts/min per IP
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
        , configTlsCert = tlsCV   -- milestone 13d; both-or-neither checked by validateConfig
        , configTlsKey  = tlsKV
        , configFetchImages = fromMaybe False (envBool mFetchImgs)  -- env-only, default False (opt-in)
        , configTmdbBaseUrl = T.pack (fromMaybe "https://api.themoviedb.org/3" mTmdbBase)
        , configTmdbImageBaseUrl = T.pack (fromMaybe "https://image.tmdb.org/t/p" mTmdbImgBase)
        , configTmdbImageSize = T.pack (fromMaybe "w500" mTmdbImgSize)
        , configTmdbBackdropSize = T.pack (fromMaybe "w1280" mTmdbBdSize)  -- m49
        , configSeriesCacheTtlSec = case mSeriesCacheTtl >>= readMaybe of
            Just n | n >= 0 -> n
            _               -> 86400  -- default 24h; 0 = always refetch (m56)
        , configScanWorkers = fromMaybe 4 (mScanW >>= readMaybe)  -- raw value; validateConfig rejects negatives (exit 2), 0 clamped to 1 below
        , configMaxBodyBytes = fromMaybe 10485760 (mMaxBody >>= readMaybe)  -- default 10 MiB; 0 = unlimited; negative rejected by validateConfig (exit 2)
        , configMemoryWarnMb = fromMaybe 1024 (mMemWarn >>= readMaybe)      -- default 1024 MB; negative rejected by validateConfig
        , configMemorySampleSecs = fromMaybe 30 (mMemSamp >>= readMaybe)    -- default 30s; 0 = disabled; negative rejected by validateConfig
        , configUiDir = scUiDir sc    -- Elm UI dir; existence checked below (exit 2)
        , configJackettUrl = T.pack <$> mJackettUrl
        , configJackettApiKey = T.pack <$> mJackettKey
        , configDelugeUrl = T.pack (fromMaybe "http://127.0.0.1:8112/json" mDelugeUrl)
        , configDelugePassword = T.pack (fromMaybe "" mDelugePw)
        , configDownloadDir = fromMaybe (stateDir </> "downloads") mDlDir
        , configRequestAutoApproveGb = fromMaybe 5 (mAutoAppr >>= readMaybe)  -- default 5 GiB; negative rejected by validateConfig
        , configIndexerUrl = T.pack <$> mIndexerUrl
        , configIndexerApiKey = T.pack <$> mIndexerKey
        , configIndexerName = T.pack <$> mIndexerNm
        }
  cfg0v <- case validateConfig cfg0 of
    Left err -> do
      hPutStrLn stderr ("config error: " ++ T.unpack err)
      exitWith (ExitFailure 2)
    Right c -> pure c
  let cfg = cfg0v { configScanWorkers = max 1 (configScanWorkers cfg0v) }  -- 0 -> 1 (sequential)
  -- Milestone 13d: validateConfig is pure, so the TLS file existence check
  -- happens here (exit 2 with a clear message when a set file is missing).
  when (isJust (configTlsCert cfg) || isJust (configTlsKey cfg)) $ do
    let paths = [ p | Just p <- [configTlsCert cfg, configTlsKey cfg] ]
    existing <- mapM doesFileExist paths
    let missing = [ p | (p, ok) <- zip paths existing, not ok ]
    when (not (null missing)) $ do
      hPutStrLn stderr ("config error: TLS cert/key file not found: " ++ unwords missing)
      exitWith (ExitFailure 2)
  -- UI dir: validateConfig is pure, so the existence check happens here
  -- (exit 2 with a clear message when a set dir is missing), like the TLS files.
  case configUiDir cfg of
    Just uiDir -> do
      ok <- doesDirectoryExist uiDir
      unless ok $ do
        hPutStrLn stderr ("config error: UI dir not found: " ++ uiDir)
        exitWith (ExitFailure 2)
    Nothing -> pure ()
  runServer cfg
