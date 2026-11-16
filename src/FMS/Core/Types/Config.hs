module FMS.Core.Types.Config
  ( Config(..)
  ) where

import Data.Text (Text)

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
  , configLogLevel    :: !Text          -- ^ minimum log level printed (env FMS_LOG_LEVEL, default \"info\"; env-only, milestone 11b)
  , configCorsOrigins :: ![Text]        -- ^ allowed CORS origins (env FMS_CORS_ORIGINS, comma-separated; empty/unset or containing \"*\" = allow all; milestone 13b)
  , configSecurityHeaders :: !Bool      -- ^ add X-Content-Type-Options/X-Frame-Options/Referrer-Policy to every response (env FMS_SECURITY_HEADERS, default True; milestone 13b)
  , configPasswordMinLength :: !Int      -- ^ minimum password length (env FMS_PASSWORD_MIN_LENGTH, default 0 = disabled; milestone 8b)
  , configMaxBitrate :: !Int             -- ^ server-wide transcode bitrate cap in bps (env FMS_MAX_BITRATE, default 0 = unlimited; milestone 6f)
  , configHwAccel    :: !Text            -- ^ transcode hardware acceleration: none|vaapi|nvenc|qsv|videotoolbox (env FMS_HWACCEL, default \"none\"; milestone 6f)
  } deriving (Show, Eq)
