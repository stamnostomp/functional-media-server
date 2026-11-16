module FMS.Modules.Sessions
  ( sessionsModule
  , MonadSessions (..)
  ) where

import FMS.Core.Module (Module (..))
import FMS.Modules.Sessions.Monad (MonadSessions (..), initSessionsState)
import FMS.Modules.Sessions.Routes (sessionsRoutes)

-- | The sessions module (docs/roadmap.org milestone 6c, docs/todo.org
-- section 6): live playback session reporting, Jellyfin-shaped
-- @/Sessions/Playing*@ + @GET /Sessions@. Init creates the in-memory
-- active-sessions TVar under \"sessions.active\" in the typed registry
-- (docs/modules.org — the core never learns about it). Sessions are not
-- persisted; the stats module keeps the durable play/progress records.
sessionsModule :: Module
sessionsModule = Module
  { moduleName   = "sessions"
  , moduleInit   = initSessionsState
  , moduleRoutes = sessionsRoutes
  }
