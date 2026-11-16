module FMS.Modules.Metadata
  ( metadataModule
  , MonadMetadata (..)
  ) where

import FMS.Core.Module (Module (..), Segment (..), get, post)
import FMS.Modules.Metadata.Monad (MonadMetadata (..), loadMetadataIntoState)
import FMS.Modules.Metadata.Routes
  ( metadataHandler
  , refreshMetadataHandler
  )

-- | The metadata module (docs/roadmap.org milestone 4, docs/todo.org
-- section 3): NFO sidecar parsing plus a disk-backed metadata cache.
-- Init loads the persisted map at @stateDir/metadata.json@ into a TVar
-- under "metadata.cache" in the typed registry — the core never learns
-- about it (docs/modules.org). Remote providers (TMDB/TvMaze) plug into
-- 'refreshMetadata' later.
metadataModule :: Module
metadataModule =
  Module
    { moduleName = "metadata"
    , moduleInit = loadMetadataIntoState
    , moduleRoutes =
        [ get  [Lit "items", Capture "id", Lit "metadata"] metadataHandler
        , post [Lit "items", Capture "id", Lit "metadata", Lit "refresh"] refreshMetadataHandler
        ]
    }
