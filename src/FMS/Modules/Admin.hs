module FMS.Modules.Admin
  ( adminModule
  , MonadAdmin (..)
  ) where

import FMS.Core.Module (Module (..))
import FMS.Modules.Admin.Monad (MonadAdmin (..), initAdminState)
import FMS.Modules.Admin.Routes (adminRoutes)
import FMS.Modules.Admin.Scheduler (startScheduler)

-- | The admin module (docs/todo.org section 11, milestone 11a): the
-- activity log (@/System/ActivityLog@, ring buffer + @stateDir/activity.log@)
-- and the scheduled-task framework (@/ScheduledTasks@, run by a forked
-- scheduler loop every 'schedulerTickSeconds'). Init creates the two TVars
-- in the typed registry, records the startup activity entry, and starts
-- the scheduler. Shutdown/restart entries are recorded by "FMS.Server".
adminModule :: Module
adminModule = Module
  { moduleName   = "admin"
  , moduleInit   = do
      initAdminState
      logActivity "Server" "server started"
      startScheduler
  , moduleRoutes = adminRoutes
  }
