module FMS.Modules.Requests
  ( requestsModule
  , MonadRequests (..)
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import System.Directory (createDirectoryIfMissing)

import FMS.Core.Module (Module (..))
import FMS.Core.Monad (envConfig)
import FMS.Core.Types.Config (Config (..))
import FMS.Core.Types.MediaFolder (MediaFolder (..))
import FMS.Modules.Library.Monad (MonadLibrary (..))
import FMS.Modules.Requests.Monad (MonadRequests (..), loadRequestsIntoState)
import FMS.Modules.Requests.Routes (requestsRoutes)

-- | The media-request queue module (m20a CRUD core + m20b tracker search /
-- Deluge push / download-dir auto-import). Requests are persisted in
-- @stateDir/requests.json@ (lowercase keys) under "requests.map".
-- Init also makes sure the download dir exists and is registered as the
-- "Downloads" media folder (so auto-imported files show up in the library).
requestsModule :: Module
requestsModule = Module
  { moduleName   = "requests"
  , moduleInit   = do
      loadRequestsIntoState
      cfg <- asks envConfig
      let dir = configDownloadDir cfg
      liftIO (createDirectoryIfMissing True dir)
      folders <- listFolders
      if any (\f -> folderPath f == dir) folders
        then pure ()
        else do
          _ <- addFolder "Downloads" dir "mixed"
          pure ()
  , moduleRoutes = requestsRoutes
  }
