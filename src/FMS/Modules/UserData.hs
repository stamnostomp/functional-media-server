module FMS.Modules.UserData
  ( userDataModule
  , MonadUserData (..)
  ) where

import FMS.Core.Module (Module (..))
import FMS.Modules.UserData.Monad (MonadUserData (..), loadUserDataIntoState)
import FMS.Modules.UserData.Routes (userDataRoutes)

-- | The user-data module (docs/roadmap.org milestone 5, docs/todo.org
-- section 9): per-user item data (played, favorite, playback position),
-- persisted as a nested JSON map at @stateDir/userdata.json@. Init loads
-- the persisted map into a TVar under \"userdata.items\" in the typed
-- registry — the core never learns about it (docs/modules.org).
userDataModule :: Module
userDataModule =
  Module
    { moduleName = "userdata"
    , moduleInit = loadUserDataIntoState
    , moduleRoutes = userDataRoutes
    }
