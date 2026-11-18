module FMS.Modules.Indexers
  ( indexersModule
  , MonadIndexers (..)
  , resolveReleaseBytes
  ) where

import FMS.Core.Module (Module (..))
import FMS.Modules.Indexers.Monad (MonadIndexers (..), loadIndexersIntoState)
import FMS.Modules.Indexers.Routes (indexersRoutes, resolveReleaseBytes)

-- | The built-in Jackett-style indexer aggregator (m21). Admins configure
-- direct Torznab/Newznab indexers (persisted in @stateDir/indexers.json@,
-- TVar under \"indexers.map\"; on first boot with FMS_INDEXER_URL +
-- FMS_INDEXER_API_KEY set, one enabled \"seed\" indexer is created);
-- users search across all enabled indexers and download .torrent files
-- through the FMS proxy so API keys and raw indexer URLs stay
-- server-side.
indexersModule :: Module
indexersModule = Module
  { moduleName   = "indexers"
  , moduleInit   = loadIndexersIntoState
  , moduleRoutes = indexersRoutes
  }
