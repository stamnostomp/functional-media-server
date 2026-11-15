module FMS.Core.Types.Config
  ( Config(..)
  ) where

-- | Server configuration. Loaded from CLI args / env vars in @app/Main.hs@;
-- more fields (stats path, API keys, cache path) land with later milestones
-- (see docs/modules.org).
data Config = Config
  { configLibraryPath :: !FilePath  -- ^ root of the media library
  , configPort        :: !Int       -- ^ HTTP port to listen on
  , configStateDir    :: !FilePath  -- ^ directory for server state
  } deriving (Show, Eq)
