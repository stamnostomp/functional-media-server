module FMS.Modules.Trackers
  ( trackersModule
  , MonadTrackers (..)
  ) where

import FMS.Core.Module (Module (..))
import FMS.Modules.Trackers.Monad (MonadTrackers (..), loadTrackersIntoState)
import FMS.Modules.Trackers.Routes (trackersRoutes)

-- | The tracker (Jackett indexer) registry + search module (m20b).
-- Trackers are persisted in @stateDir/trackers.json@ and kept in a TVar
-- under "trackers.map"; on first boot with FMS_JACKETT_URL +
-- FMS_JACKETT_API_KEY set, one enabled "jackett" tracker is seeded.
-- SECURITY: the ApiKey is only sent to the tracker itself and redacted
-- from non-admin API responses.
trackersModule :: Module
trackersModule = Module
  { moduleName   = "trackers"
  , moduleInit   = loadTrackersIntoState
  , moduleRoutes = trackersRoutes
  }
