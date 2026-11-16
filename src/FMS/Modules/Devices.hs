module FMS.Modules.Devices
  ( devicesModule
  , MonadDevices (..)
  ) where

import FMS.Core.Module (Module (..))
import FMS.Modules.Devices.Monad (MonadDevices (..), loadDevicesIntoState)
import FMS.Modules.Devices.Routes (devicesRoutes)

-- | The client-device registry module (milestone 10c, docs/todo.org
-- section 8, Jellyfin =/Devices=): self-reported devices are upserted,
-- listed, fetched and deleted; persisted in @stateDir/devices.json@ and
-- kept in a TVar under "devices.map" in the typed registry (the core
-- never learns about it, docs/modules.org). Every mutation is persisted
-- back (the Stats/Metadata pattern).
devicesModule :: Module
devicesModule = Module
  { moduleName   = "devices"
  , moduleInit   = loadDevicesIntoState
  , moduleRoutes = devicesRoutes
  }
