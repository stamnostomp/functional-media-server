module FMS.Core.Types.Config
  ( Config(..)
  , normalizeHwAccel
  , validateConfig
  ) where

import Data.Char (toLower)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T

-- | Server configuration. Loaded from CLI args / env vars in @app/Main.hs@;
-- more fields (stats path, cache path) land with later milestones
-- (see docs/modules.org).
data Config = Config
  { configLibraryPath :: !FilePath  -- ^ root of the media library
  , configPort        :: !Int       -- ^ HTTP port to listen on
  , configStateDir    :: !FilePath  -- ^ directory for server state
  , configRequireAuth :: !Bool      -- ^ when True, every non-open path requires a valid token (env FMS_REQUIRE_AUTH, default False)
  , configServerName  :: !Text      -- ^ display name (FMS_SERVER_NAME / config.json; milestone 1b)
  , configFollowSymlinks :: !Bool   -- ^ scanner symlink policy (env FMS_FOLLOW_SYMLINKS, default False; milestone 2b)
  , configTmdbApiKey  :: !Text      -- ^ TMDB API key for the remote metadata fallback (env MEDIA_TMDB_API_KEY, milestone 4c; empty disables it)
  , configRateLimit   :: !(Maybe Int)  -- ^ max requests per minute per client IP (env FMS_RATE_LIMIT; Nothing/0 = disabled; milestone 14a)
  , configRateLimitAuth :: !(Maybe Int) -- ^ stricter per-IP bucket for POST /AuthenticateByName (env FMS_RATE_LIMIT_AUTH; default Just 10; Nothing/0 = disabled; brute-force blunting)
  , configLogLevel    :: !Text          -- ^ minimum log level printed (env FMS_LOG_LEVEL, default \"info\"; env-only, milestone 11b)
  , configCorsOrigins :: ![Text]        -- ^ allowed CORS origins (env FMS_CORS_ORIGINS, comma-separated; empty/unset or containing \"*\" = allow all; milestone 13b)
  , configSecurityHeaders :: !Bool      -- ^ add X-Content-Type-Options/X-Frame-Options/Referrer-Policy to every response (env FMS_SECURITY_HEADERS, default True; milestone 13b)
  , configPasswordMinLength :: !Int      -- ^ minimum password length (env FMS_PASSWORD_MIN_LENGTH, default 0 = disabled; milestone 8b)
  , configMaxBitrate :: !Int             -- ^ server-wide transcode bitrate cap in bps (env FMS_MAX_BITRATE, default 0 = unlimited; milestone 6f)
  , configHwAccel    :: !Text            -- ^ transcode hardware acceleration: none|vaapi|nvenc|qsv|videotoolbox (env FMS_HWACCEL, default \"none\"; milestone 6f)
  , configWatch      :: !Bool            -- ^ inotify auto-rescan on media changes (env FMS_WATCH, default False; milestone 2d)
  , configMaxTranscodes :: !Int         -- ^ max concurrent ffmpeg jobs (env FMS_MAX_TRANSCODES, default 2, <=0 -> 2; milestone 15)
  , configFfmpegTimeoutSec :: !Int      -- ^ kill an ffmpeg job after this many seconds (env FMS_FFMPEG_TIMEOUT, default 30, <=0 -> 30; milestone 15)
  , configTlsCert :: !(Maybe FilePath)  -- ^ TLS certificate PEM (env FMS_TLS_CERT / --tls-cert; milestone 13d)
  , configTlsKey  :: !(Maybe FilePath)  -- ^ TLS private key PEM (env FMS_TLS_KEY / --tls-key; milestone 13d)
  , configFetchImages :: !Bool          -- ^ download artwork from TMDB during metadata refresh (env FMS_FETCH_IMAGES, default False = opt-in; artwork milestone)
  , configTmdbBaseUrl :: !Text          -- ^ TMDB v3 API base (env FMS_TMDB_BASE_URL, default https://api.themoviedb.org/3)
  , configTmdbImageBaseUrl :: !Text     -- ^ TMDB image CDN base (env FMS_TMDB_IMAGE_BASE_URL, default https://image.tmdb.org/t/p)
  , configTmdbImageSize :: !Text        -- ^ TMDB image size (env FMS_TMDB_IMAGE_SIZE, default w500)
  , configScanWorkers :: !Int            -- ^ parallel worker threads for the per-file stat phase of a library scan (env FMS_SCAN_WORKERS, default 4, min 1 = sequential)
  , configMaxBodyBytes :: !Int           -- ^ max request body size in bytes; larger bodies get 413 (env FMS_MAX_BODY_BYTES, default 10485760 = 10 MiB; 0 = unlimited)
  , configMemoryWarnMb :: !Int           -- ^ RSS warn threshold in MB (env FMS_MEMORY_WARN_MB, default 1024; memory monitoring)
  , configMemorySampleSecs :: !Int       -- ^ RSS sampling interval in seconds (env FMS_MEMORY_SAMPLE_SECS, default 30; 0 = disabled)
  } deriving (Show, Eq)

-- | Normalise a hardware-acceleration name (milestone 6f): case-
-- insensitive; anything but vaapi/nvenc/qsv/videotoolbox becomes
-- @\"none\"@. Pure. (Lives here rather than in the Transcode module so
-- 'validateConfig' can use it without an import cycle.)
normalizeHwAccel :: Text -> Text
normalizeHwAccel t =
  let l = T.toLower (T.strip t)
  in if l `elem` ["vaapi", "nvenc", "qsv", "videotoolbox"] then l else "none"

-- | Validate a fully-assembled 'Config' (milestone 1d): port in
-- 1..65535, non-empty library path and state dir, known log level
-- (debug/info/warn/error, case-insensitive — normalised to lowercase),
-- non-empty CORS origin list, normalised hwaccel, non-negative password
-- minimum length and max bitrate. Returns the (possibly normalised)
-- config on 'Right', or an ASCII 'Left' message the entry point prints
-- before exiting with code 2.
validateConfig :: Config -> Either Text Config
validateConfig cfg
  | configPort cfg < 1 || configPort cfg > 65535 =
      Left ("invalid port " <> T.pack (show (configPort cfg))
            <> " (must be 1..65535)")
  | null (configLibraryPath cfg) =
      Left "library path must not be empty"
  | null (configStateDir cfg) =
      Left "state dir must not be empty"
  | not (logLvl `elem` ["debug", "info", "warn", "error"]) =
      Left ("invalid log level: " <> configLogLevel cfg
            <> " (expected debug|info|warn|error)")
  | null (configCorsOrigins cfg) =
      Left "CORS origins list must not be empty"
  | configPasswordMinLength cfg < 0 =
      Left "password min length must be >= 0"
  | configMaxBitrate cfg < 0 =
      Left "max bitrate must be >= 0"
  | maybe False (< 0) (configRateLimitAuth cfg) =
      Left "auth rate limit must be >= 0"
  | configScanWorkers cfg < 0 =
      Left "scan workers must be >= 0"
  | configMaxBodyBytes cfg < 0 =
      Left "max body bytes must be >= 0"
  | configMemoryWarnMb cfg < 0 =
      Left "memory warn threshold must be >= 0 MB"
  | configMemorySampleSecs cfg < 0 =
      Left "memory sample seconds must be >= 0"
  | configMaxTranscodes cfg < 1 =
      Left "max transcodes must be >= 1"
  | configFfmpegTimeoutSec cfg < 1 =
      Left "ffmpeg timeout must be >= 1 second"
  | isJust (configTlsCert cfg) /= isJust (configTlsKey cfg) =
      Left "TLS cert and key must be set together (FMS_TLS_CERT / FMS_TLS_KEY)"
  | otherwise = Right cfg
      { configLogLevel = T.pack logLvl
      , configHwAccel  = normalizeHwAccel (configHwAccel cfg)
      }
  where
    logLvl = map toLower (T.unpack (configLogLevel cfg))
