module FMS.Modules.Admin.Tasks
  ( defaultTasks
  , runTaskAction
  ) where

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Acid (AcidState, createCheckpoint)
import Data.Text (Text)
import qualified Data.Text as T

import FMS.Core.Monad (AppM, getState)
import FMS.Core.Types.MediaItem (MediaItem (..))
import FMS.Modules.Admin.Types (ScheduledTask (..))
import FMS.Modules.Library.Monad (MonadLibrary (..), libraryAcidKey)
import FMS.Modules.Library.State (LibraryState)
import FMS.Modules.Metadata.Monad (MonadMetadata (..))

-- | The built-in scheduled tasks (docs/todo.org section 11, milestone
-- 11a): library rescan every 5 minutes, metadata refresh every hour,
-- acid-state checkpoint every 10 minutes. All enabled by default; a task
-- that has never run is due immediately (see 'taskDue').
defaultTasks :: [ScheduledTask]
defaultTasks =
  [ ScheduledTask
      { stId = "library-rescan", stName = "Library rescan"
      , stIntervalSeconds = 300, stEnabled = True
      , stLastRun = Nothing, stLastResult = Nothing
      }
  , ScheduledTask
      { stId = "metadata-refresh", stName = "Metadata refresh"
      , stIntervalSeconds = 3600, stEnabled = True
      , stLastRun = Nothing, stLastResult = Nothing
      }
  , ScheduledTask
      { stId = "acid-checkpoint", stName = "Acid-state checkpoint"
      , stIntervalSeconds = 600, stEnabled = True
      , stLastRun = Nothing, stLastResult = Nothing
      }
  ]

-- | Run one task's action and return a short human-readable result
-- message. Unknown ids are reported (not thrown) -- 'runTask' only calls
-- this for registered tasks anyway. Errors inside an action surface via
-- the 'AppError' channel of 'runApp' in the caller and are recorded in
-- 'stLastResult' as an \"error: ...\" message.
runTaskAction :: ScheduledTask -> AppM Text
runTaskAction t = case stId t of
  "library-rescan" -> do
    rescanLibrary
    pure "library rescan finished"
  "metadata-refresh" -> do
    items <- listItems
    results <- forM items (refreshMetadata . itemId)
    let n = length [ () | Just _ <- results ]
    pure (T.pack (show n ++ " of " ++ show (length items) ++ " item(s) refreshed"))
  "acid-checkpoint" -> do
    mAcid <- getState libraryAcidKey :: AppM (Maybe (AcidState LibraryState))
    case mAcid of
      Just acid -> do
        liftIO (createCheckpoint acid)   -- checkpoint but keep the state OPEN
        pure "acid-state checkpoint created"
      Nothing -> pure "no library acid-state registered"
  other -> pure ("unknown task id: " <> other)
