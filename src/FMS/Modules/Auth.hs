module FMS.Modules.Auth
  ( authModule
  , MonadAuth (..)
  ) where

import FMS.Core.Module (Module (..))
import FMS.Modules.Auth.Monad (MonadAuth (..), initAuthState)
import FMS.Modules.Auth.Routes (authRoutes)

-- | The auth module (docs/roadmap.org milestone 5, docs/todo.org section 8):
-- users, bcrypt-hashed passwords, session tokens and API keys, Jellyfin-style
-- endpoints. Init loads (or bootstraps) @stateDir/users.json@ into a TVar
-- under "auth.users" and creates the in-memory "auth.tokens" TVar — the
-- router's auth gate (FMS_REQUIRE_AUTH) reads those two keys from the typed
-- registry, so the core never learns about the module (docs/modules.org).
authModule :: Module
authModule =
  Module
    { moduleName = "auth"
    , moduleInit = initAuthState
    , moduleRoutes = authRoutes
    }
