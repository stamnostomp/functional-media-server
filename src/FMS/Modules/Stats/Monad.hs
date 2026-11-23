module FMS.Modules.Stats.Monad
  ( MonadStats (..)
  , statsStateKey
  , statsFilePath
  , loadStatsIntoState
  , modifyStats
  , applyPlay
  , applyProgress
  , continueWatching
  , popular
  ) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import Data.Time (UTCTime, getCurrentTime)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import FMS.Core.Monad (AppEnv (..), AppM, getState, putState)
import FMS.Core.Types.Config (Config (..))
import FMS.Core.Types.Progress (ItemStats (..), Progress (..))
import FMS.Modules.Stats.Persistence (loadStats, saveStats)

-- | Namespaced key under which the stats module stashes its
-- @TVar (HashMap Text ItemStats)@ in the typed registry
-- (docs/modules.org: the core never knows module state).
statsStateKey :: Text
statsStateKey = "stats.items"

-- | Where the stats map is persisted: @stateDir/stats.json@.
statsFilePath :: Config -> FilePath
statsFilePath cfg = configStateDir cfg </> "stats.json"

-- | Capability for recording and reading playback stats
-- (docs/modules.org milestone 5). Handlers in other modules (e.g.
-- "FMS.Modules.Stream") can require @MonadStats m => ...@.
class Monad m => MonadStats m where
  recordPlay     :: Text -> m ()                              -- ^ bump play count + lastPlayedAt
  recordProgress :: Text -> Double -> Double -> m ()          -- ^ set progress (currentTime, duration)
  getAllStats    :: m (HashMap Text ItemStats)
  getItemStats   :: Text -> m (Maybe ItemStats)

instance MonadStats AppM where
  recordPlay itemIdent = do
    now <- liftIO getCurrentTime
    let fresh = ItemStats {statsPlayCount = 1, statsLastPlayedAt = now, statsProgress = Nothing}
    modifyStats (HM.insertWith (\_new old -> applyPlay now old) itemIdent fresh)

  recordProgress itemIdent currentTime duration = do
    now <- liftIO getCurrentTime
    let p     = Progress currentTime duration now
        fresh = ItemStats {statsPlayCount = 0, statsLastPlayedAt = now, statsProgress = Just p}
    modifyStats (HM.insertWith (\_new old -> applyProgress p old) itemIdent fresh)

  getAllStats = do
    mTv <- getState statsStateKey
    case mTv of
      Just tv -> liftIO (readTVarIO tv)
      Nothing -> pure HM.empty

  getItemStats itemIdent = HM.lookup itemIdent <$> getAllStats

-- | Pure semantics of 'recordPlay': bump the play count and stamp the
-- last-played time, leaving any recorded progress untouched.
applyPlay :: UTCTime -> ItemStats -> ItemStats
applyPlay now s =
  s { statsPlayCount = statsPlayCount s + 1
    , statsLastPlayedAt = now
    }

-- | Pure semantics of 'recordProgress': set/overwrite the progress,
-- leaving the play count untouched.
applyProgress :: Progress -> ItemStats -> ItemStats
applyProgress p s = s {statsProgress = Just p}

-- | Items with recorded progress that is not yet finished
-- (@currentTime < duration * 0.95@), most recently reported first.
continueWatching :: HashMap Text ItemStats -> [(Text, Progress)]
continueWatching stats =
  sortOn (Down . progressUpdatedAt . snd)
    [ (ident, p)
    | (ident, s) <- HM.toList stats
    , Just p <- [statsProgress s]
    , progressCurrentTime p > 0                              -- m89: 0% is not "continue"
    , progressCurrentTime p < progressDuration p * 0.95
    ]

-- | All items with their stats, most-played first.
popular :: HashMap Text ItemStats -> [(Text, ItemStats)]
popular = sortOn (Down . statsPlayCount . snd) . HM.toList

-- | Apply an update to the stats TVar AND persist the result to
-- @stateDir/stats.json@ after every update (the sibling-repo pattern:
-- in-memory state for reads, JSON on disk for restarts).
modifyStats :: (HashMap Text ItemStats -> HashMap Text ItemStats) -> AppM ()
modifyStats f = do
  cfg <- asks envConfig
  tv <- ensureStatsTVar
  updated <- liftIO $ atomically $ do
    modifyTVar' tv f
    readTVar tv
  liftIO (saveStats (statsFilePath cfg) updated)

ensureStatsTVar :: AppM (TVar (HashMap Text ItemStats))
ensureStatsTVar = do
  mTv <- getState statsStateKey
  case mTv of
    Just tv -> pure tv
    Nothing -> do
      tv <- liftIO (newTVarIO HM.empty)
      putState statsStateKey tv
      pure tv

-- | Reads the persisted stats file into a fresh TVar under 'statsStateKey'
-- (creating the state dir if needed). Called once from the stats module's
-- @moduleInit@.
loadStatsIntoState :: AppM ()
loadStatsIntoState = do
  cfg <- asks envConfig
  liftIO (createDirectoryIfMissing True (configStateDir cfg))
  loaded <- liftIO (loadStats (statsFilePath cfg))
  tv <- liftIO (newTVarIO loaded)
  putState statsStateKey tv
