module FMS.Modules.Library
  ( libraryModule
  , MonadLibrary(..)
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Acid (openLocalStateFrom)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import FMS.Core.Module (Module(..), Segment(..), get)
import FMS.Core.Monad (MonadLog(..), envConfig, putState)
import FMS.Core.Types.Config (Config(..))
import FMS.Modules.Library.Monad (MonadLibrary(..), libraryAcidKey)
import FMS.Modules.Library.Routes (itemsHandler, libraryRoutes)
import FMS.Modules.Library.State (initialLibraryState)

-- | The library module (docs/roadmap.org milestone 2). Init opens the
-- acid-state at @configStateDir \/ \"acid-state\" \/ \"library\"@ and stashes
-- the handle in the typed registry under "library.acid" — the core never
-- learns about module state (docs/modules.org) — then does an initial scan.
libraryModule :: Module
libraryModule = Module
  { moduleName = "library"
  , moduleInit = do
      cfg <- asks envConfig
      let acidDir = configStateDir cfg </> "acid-state" </> "library"
      liftIO (createDirectoryIfMissing True acidDir)
      acid <- liftIO (openLocalStateFrom acidDir initialLibraryState)
      putState libraryAcidKey acid
      rescanLibrary
      logInfo "library module initialised"
    -- Milestone 4a (docs/todo.org section 4): the Jellyfin-shaped items
    -- query endpoint lives next to the plain /library routes.
  , moduleRoutes = libraryRoutes ++ [get [Lit "items"] (\captures req -> itemsHandler captures req)]
  }
