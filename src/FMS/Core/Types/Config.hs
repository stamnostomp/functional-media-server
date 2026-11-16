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
  } deriving (Show, Eq)
