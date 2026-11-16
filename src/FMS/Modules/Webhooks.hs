module FMS.Modules.Webhooks
  ( webhooksModule
  , MonadWebhooks (..)
  , fireWebhooks
  ) where

import FMS.Core.Module (Module (..))
import FMS.Modules.Webhooks.Monad (MonadWebhooks (..), fireWebhooks, loadWebhooksIntoState)
import FMS.Modules.Webhooks.Routes (webhooksRoutes)

-- | The webhooks module (milestone 12, docs/todo.org section 12): clients
-- register URLs that the server POSTs JSON events to (PlaybackStart /
-- PlaybackProgress / PlaybackStop / LibraryChanged / UserDataSaved / ...).
-- Persisted in @stateDir/webhooks.json@, kept in a TVar under
-- "webhooks.map" in the typed registry; every mutation is persisted back.
webhooksModule :: Module
webhooksModule = Module
  { moduleName   = "webhooks"
  , moduleInit   = loadWebhooksIntoState
  , moduleRoutes = webhooksRoutes
  }
