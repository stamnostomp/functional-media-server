module FMS.Modules.Requests
  ( requestsModule
  , MonadRequests (..)
  ) where

import FMS.Core.Module (Module (..))
import FMS.Modules.Requests.Monad (MonadRequests (..), loadRequestsIntoState)
import FMS.Modules.Requests.Routes (requestsRoutes)

-- | The media-request queue module (m20a CRUD core, Radarr/Sonarr-style;
-- Jackett/Deluge integration comes in m20b). Requests are persisted in
-- @stateDir/requests.json@ (lowercase keys) and kept in a TVar under
-- "requests.map" in the typed registry; the API JSON is PascalCase.
requestsModule :: Module
requestsModule = Module
  { moduleName   = "requests"
  , moduleInit   = loadRequestsIntoState
  , moduleRoutes = requestsRoutes
  }
