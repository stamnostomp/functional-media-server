module FMS.Modules.Requests
  ( requestsModule
  , MonadRequests (..)
  ) where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing)

import FMS.Core.Module (Module (..))
import FMS.Core.Types.MediaFolder (MediaFolder (..))
import FMS.Modules.Library.Monad (MonadLibrary (..))
import FMS.Modules.Requests.Janitor (startRequestsJanitor)
import FMS.Modules.Requests.Monad (MonadRequests (..), getRequestSettings, loadRequestSettingsIntoState, loadRequestsIntoState)
import FMS.Modules.Requests.Routes (requestsRoutes)
import FMS.Modules.Requests.Types (rsDownloadDir)

-- | The media-request queue module (m20a CRUD core + m20b tracker search /
-- Deluge push / download-dir auto-import; m27 runtime download-client
-- settings in @stateDir/requests-settings.json@). Requests are persisted
-- in @stateDir/requests.json@ (lowercase keys) under "requests.map".
-- Init also makes sure the EFFECTIVE download dir exists and is registered
-- as the "Downloads" media folder (so auto-imported files show up in the
-- library).
requestsModule :: Module
requestsModule = Module
  { moduleName   = "requests"
  , moduleInit   = do
      loadRequestsIntoState
      loadRequestSettingsIntoState          -- m27: seed from env on first boot
      s <- getRequestSettings
      let dir = T.unpack (rsDownloadDir s)
      liftIO (createDirectoryIfMissing True dir)
      folders <- listFolders
      if any (\f -> folderPath f == dir) folders
        then pure ()
        else do
          _ <- addFolder "Downloads" dir "mixed"
          pure ()
      startRequestsJanitor   -- m82: re-add stalled (0% / 0-seed) torrents every 60s
  , moduleRoutes = requestsRoutes
  }
