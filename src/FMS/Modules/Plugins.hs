module FMS.Modules.Plugins
  ( pluginsModule
  , MonadPlugins (..)
  ) where

import FMS.Core.Module (Module (..))
import FMS.Modules.Plugins.Monad (MonadPlugins (..), loadPluginsIntoState)
import FMS.Modules.Plugins.Routes (pluginsRoutes)

-- | The plugin registry skeleton module (plugin system, docs/todo.org
-- section on plugins; Jellyfin =/Plugins= parity). This is a registry,
-- not dynamic loading: built-in plugin entries with persisted
-- enable/disable state in @stateDir/plugins.json@ (kept in a TVar under
-- "plugins.map" in the typed registry), plus an in-process hook registry
-- under "plugins.hooks" (hook name -> plugin ids) that other modules can
-- use later via 'FMS.Modules.Plugins.Types.registerHook'.
pluginsModule :: Module
pluginsModule = Module
  { moduleName   = "plugins"
  , moduleInit   = loadPluginsIntoState
  , moduleRoutes = pluginsRoutes
  }
