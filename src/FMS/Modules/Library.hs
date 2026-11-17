module FMS.Modules.Library
  ( libraryModule
  , MonadLibrary(..)
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Acid (openLocalStateFrom)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import FMS.Core.Module (Module(..), Segment(..), get, post)
import FMS.Core.Monad (MonadLog(..), envConfig, putState)
import FMS.Core.Types.Config (Config(..))
import FMS.Modules.Library.Monad (MonadLibrary(..), initFolders, libraryAcidKey)
import FMS.Modules.Library.Routes
  ( ancestorsHandler
  , itemDetailHandler
  , itemsCountsHandler
  , itemsHandler
  , libraryRoutes
  , mediaSourcesHandler
  , searchHintsHandler
  , similarHandler
  , specialFeaturesHandler
  )
import FMS.Modules.Library.State (initialLibraryState)
import FMS.Modules.Metadata.Routes (refreshAllHandler, remoteSearchHandler, unmatchedHandler)
import FMS.Modules.Library.Watch (startWatcher)

-- | The library module (docs/roadmap.org milestone 2, milestone 2c media
-- folders). Init opens the acid-state at @configStateDir \/ \"acid-state\"
-- \/ \"library\"@ and stashes the handle in the typed registry under
-- "library.acid" — the core never learns about module state
-- (docs/modules.org) — loads the media folders from
-- @stateDir/folders.json@ (default: one folder at the configured library
-- path) into the "library.folders" TVar, then does an initial scan of
-- every folder.
libraryModule :: Module
libraryModule = Module
  { moduleName = "library"
  , moduleInit = do
      cfg <- asks envConfig
      let acidDir = configStateDir cfg </> "acid-state" </> "library"
      liftIO (createDirectoryIfMissing True acidDir)
      acid <- liftIO (openLocalStateFrom acidDir initialLibraryState)
      putState libraryAcidKey acid
      initFolders
      rescanLibrary
      startWatcher   -- milestone 2d: inotify auto-rescan behind FMS_WATCH
      logInfo "library module initialised"
    -- Milestone 4a (docs/todo.org section 4): the Jellyfin-shaped items
    -- query endpoint lives next to the plain /library routes. Milestone
    -- 4e: /items/Counts and /items/:id and /items/:id/Ancestors and
    -- /Search/Hints. Routes are first-match wins, so the literal
    -- [Lit "items", Lit "Counts"] MUST come before [Lit "items", Capture
    -- "id"].
  , moduleRoutes = libraryRoutes
      ++ [ get [Lit "items"] (\captures req -> itemsHandler captures req)
         , get [Lit "items", Lit "Counts"] itemsCountsHandler
         -- milestone 4m: literal segments BEFORE the [items, Capture "id"]
         -- capture route (first match wins); handlers live in Metadata.Routes
         , get [Lit "items", Lit "RemoteSearch"] remoteSearchHandler
         , get [Lit "items", Lit "Unmatched"] unmatchedHandler
         , post [Lit "Library", Lit "Refresh"] refreshAllHandler
         , get [Lit "items", Capture "id"] itemDetailHandler
         , get [Lit "items", Capture "id", Lit "Ancestors"] ancestorsHandler
         , get [Lit "items", Capture "id", Lit "SpecialFeatures"] specialFeaturesHandler
         , get [Lit "items", Capture "id", Lit "Similar"] similarHandler
         , get [Lit "items", Capture "id", Lit "MediaSources"] mediaSourcesHandler
         , get [Lit "Search", Lit "Hints"] searchHintsHandler
         ]
  }
