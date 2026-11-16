module FMS.Modules.Collections
  ( collectionsModule
  , MonadCollections (..)
  ) where

import FMS.Core.Module (Module (..))
import FMS.Modules.Collections.Monad (MonadCollections (..), loadCollectionsIntoState)
import FMS.Modules.Collections.Routes (collectionsRoutes)

-- | The collections (box sets) module (milestone 7): create/list/get/
-- delete collections and add/remove/list their items, Jellyfin-shaped
-- (@/Collections...@), mirroring the playlists module. Collections also
-- surface as BoxSet pseudo-items in @/items@ (see
-- "FMS.Modules.Collections.Types.boxSetItemValue"). Init loads the
-- persisted map at @stateDir/collections.json@ into a TVar under
-- "collections.map" in the typed registry — the core never learns about
-- it (docs/modules.org) — and every mutation is persisted back (the
-- Stats/Metadata pattern).
collectionsModule :: Module
collectionsModule = Module
  { moduleName   = "collections"
  , moduleInit   = loadCollectionsIntoState
  , moduleRoutes = collectionsRoutes
  }
