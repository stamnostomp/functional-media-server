module FMS.Modules.Admin.Scheduler
  ( startScheduler
  , schedulerTickSeconds
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forM_, forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.Time.Clock (getCurrentTime)

import FMS.Core.Monad (AppEnv, AppM, runApp)
import FMS.Modules.Admin.Monad (MonadAdmin (..), taskDue)
import FMS.Modules.Admin.Types (ScheduledTask (..))

-- | How often the scheduler wakes up to look for due tasks.
schedulerTickSeconds :: Int
schedulerTickSeconds = 10

-- | Fork the scheduler loop (docs/todo.org section 11, milestone 11a).
-- Called once from the admin module's @moduleInit@; the thread id is not
-- kept -- the loop is a daemon that lives as long as the process.
startScheduler :: AppM ()
startScheduler = do
  env <- ask
  _ <- liftIO (forkIO (schedulerLoop env))
  pure ()

-- | The scheduler loop: sleep one tick, then run every due task inside
-- 'runApp' (so a crashing task cannot kill the loop).
schedulerLoop :: AppEnv -> IO ()
schedulerLoop env = forever $ do
  threadDelay (schedulerTickSeconds * 1000000)
  _ <- runApp env schedulerTick
  pure ()

-- | One scheduler tick: find due tasks and run each via the
-- 'MonadAdmin.runTask' capability (which stamps LastRun/LastResult and
-- writes an activity entry).
schedulerTick :: AppM ()
schedulerTick = do
  now   <- liftIO getCurrentTime
  tasks <- listTasks
  forM_ (filter (taskDue now) tasks) $ \t -> do
    _ <- runTask (stId t)
    pure ()
