module FMS.Modules.Admin.Monad
  ( MonadAdmin (..)
  , adminActivityKey
  , adminTasksKey
  , activityLogCap
  , activityLogFilePath
  , initAdminState
  , pushActivity
  , taskDue
  ) where

import Control.Concurrent.STM
  ( TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, writeTVar )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, asks)
import Data.Aeson (encode)
import qualified Data.ByteString as BS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import qualified Data.ByteString.Lazy as LBS
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import FMS.Core.Monad (AppEnv (..), AppM, envConfig, getState, putState, runApp)
import FMS.Core.Types.Config (Config (..))
import FMS.Modules.Admin.Tasks (defaultTasks, runTaskAction)
import FMS.Modules.Admin.Types (ActivityEntry (..), ScheduledTask (..))

-- | Registry key for the activity ring buffer:
-- @TVar (Int, [ActivityEntry])@ = (next id, entries oldest-first).
adminActivityKey :: Text
adminActivityKey = "admin.activity"

-- | Registry key for the scheduled tasks: @TVar (HashMap Text ScheduledTask)@.
adminTasksKey :: Text
adminTasksKey = "admin.tasks"

-- | The in-memory activity ring buffer keeps at most this many entries
-- (oldest are dropped first); every entry is also appended to
-- 'activityLogFilePath' so nothing is lost to the cap on disk.
activityLogCap :: Int
activityLogCap = 500

-- | Where activity entries are appended as JSON lines:
-- @stateDir/activity.log@.
activityLogFilePath :: Config -> FilePath
activityLogFilePath cfg = configStateDir cfg </> "activity.log"

-- | Capability for the admin module (docs/todo.org section 11, milestone
-- 11a): activity log + scheduled tasks. Handlers require
-- @MonadAdmin m => ...@.
class Monad m => MonadAdmin m where
  logActivity  :: Text -> Text -> m ()                 -- ^ type, message
  listActivity :: m [ActivityEntry]                     -- ^ newest first
  listTasks    :: m [ScheduledTask]                     -- ^ sorted by id
  runTask      :: Text -> m (Maybe ScheduledTask)       -- ^ run now, stamp LastRun/LastResult
  enableTask   :: Text -> Bool -> m Bool                -- ^ False when the id is unknown

instance MonadAdmin AppM where
  logActivity typ msg = do
    now <- liftIO getCurrentTime
    cfg <- asks envConfig
    tv  <- activityTVar
    stored <- liftIO $ atomically $ do
      (nextId, buf) <- readTVar tv
      let (nextId', buf') = pushActivity nextId buf (ActivityEntry 0 now typ msg)
      writeTVar tv (nextId', buf')
      pure (ActivityEntry nextId now typ msg)
    liftIO (appendActivityLine cfg stored)

  listActivity = do
    tv <- activityTVar
    reverse . snd <$> liftIO (readTVarIO tv)

  listTasks = do
    tv <- tasksTVar
    sortOn stId . HM.elems <$> liftIO (readTVarIO tv)

  runTask tid = do
    env <- ask
    tv  <- tasksTVar
    mT  <- liftIO (HM.lookup tid <$> readTVarIO tv)
    case mT of
      Nothing -> pure Nothing
      Just t -> do
        now  <- liftIO getCurrentTime
        -- Run the action in a nested runApp so a failing task (e.g. a
        -- probe error) is recorded as an "error: ..." result instead of
        -- failing the caller.
        eres <- liftIO (runApp env (runTaskAction t))
        let (ok, msg) = case eres of
              Right m -> (True, m)
              Left e  -> (False, T.pack (show e))
            t' = t { stLastRun    = Just now
                   , stLastResult = Just (if ok then "ok: " <> msg else "error: " <> msg)
                   }
        liftIO (atomically (modifyTVar' tv (HM.insert tid t')))
        logActivity "TaskRun"
          (stId t' <> " (" <> stName t' <> "): " <> fromMaybe "" (stLastResult t'))
        pure (Just t')

  enableTask tid enabled = do
    tv <- tasksTVar
    liftIO $ atomically $ do
      m <- HM.lookup tid <$> readTVar tv
      case m of
        Nothing -> pure False
        Just t  -> do
          modifyTVar' tv (HM.insert tid t { stEnabled = enabled })
          pure True

-- | Pure: push one entry into the activity ring buffer. The entry's id is
-- set to @nextId@ and the counter is bumped; the buffer keeps at most
-- 'activityLogCap' entries, dropping the oldest.
pushActivity
  :: Int -> [ActivityEntry] -> ActivityEntry -> (Int, [ActivityEntry])
pushActivity nextId buf e =
  let buf' = buf ++ [e { aeId = nextId }]
  in (nextId + 1, drop (max 0 (length buf' - activityLogCap)) buf')

-- | Pure: is this task due at @now@? Enabled, and either never run or
-- @lastRun + interval <= now@ (boundary inclusive).
taskDue :: UTCTime -> ScheduledTask -> Bool
taskDue now t =
  stEnabled t &&
  case stLastRun t of
    Nothing -> True
    Just lr -> diffUTCTime now lr >= fromIntegral (stIntervalSeconds t)

-- | Module init: create the two TVars in the typed registry (activity ring
-- buffer starts empty with counter 1 -- the log file is the durable
-- record, we do not replay it on boot; tasks start at 'defaultTasks').
initAdminState :: AppM ()
initAdminState = do
  cfg <- asks envConfig
  liftIO (createDirectoryIfMissing True (configStateDir cfg))
  atv <- liftIO (newTVarIO ((1, []) :: (Int, [ActivityEntry])))
  putState adminActivityKey atv
  ttv <- liftIO (newTVarIO (HM.fromList [ (stId t, t) | t <- defaultTasks ]
                           :: HashMap Text ScheduledTask))
  putState adminTasksKey ttv

-- Internals ---------------------------------------------------------------

activityTVar :: AppM (TVar (Int, [ActivityEntry]))
activityTVar = do
  m <- getState adminActivityKey
  case m of
    Just tv -> pure tv
    Nothing -> do
      tv <- liftIO (newTVarIO ((1, []) :: (Int, [ActivityEntry])))
      putState adminActivityKey tv
      pure tv

tasksTVar :: AppM (TVar (HashMap Text ScheduledTask))
tasksTVar = do
  m <- getState adminTasksKey
  case m of
    Just tv -> pure tv
    Nothing -> do
      tv <- liftIO (newTVarIO (HM.fromList [ (stId t, t) | t <- defaultTasks ]
                               :: HashMap Text ScheduledTask))
      putState adminTasksKey tv
      pure tv

-- | Append one activity entry to @stateDir/activity.log@ as a JSON line.
appendActivityLine :: Config -> ActivityEntry -> IO ()
appendActivityLine cfg e =
  BS.appendFile (activityLogFilePath cfg) (LBS.toStrict (encode e) <> "\n")
