module FMS.Modules.Trackers.Monad
  ( MonadTrackers (..)
  , trackersStateKey
  , trackersFilePath
  , loadTrackersIntoState
  , newTrackerId
  ) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.Random (newStdGen, randomRs)

import FMS.Core.Monad (AppM, envConfig, getState, putState)
import FMS.Core.Types.Config (Config (..))
import FMS.Modules.Trackers.Persistence (loadTrackers, saveTrackers)
import FMS.Modules.Trackers.Types (Tracker (..))

-- | Namespaced key under which the trackers module stashes its
-- @TVar (HashMap Text Tracker)@ in the typed registry.
trackersStateKey :: Text
trackersStateKey = "trackers.map"

-- | Where the tracker map is persisted: @stateDir/trackers.json@.
trackersFilePath :: Config -> FilePath
trackersFilePath cfg = configStateDir cfg </> "trackers.json"

-- | Capability for the tracker (Jackett indexer) registry (m20b).
class Monad m => MonadTrackers m where
  listTrackers      :: m [Tracker]                    -- ^ all, sorted by Id
  getTracker        :: Text -> m (Maybe Tracker)       -- ^ by id
  addTracker        :: Text -> Text -> Text -> Bool -> m Tracker
    -- ^ name, url, apiKey, enabled
  deleteTracker     :: Text -> m Bool                  -- ^ id -> existed?
  setTrackerEnabled :: Text -> Bool -> m Bool          -- ^ id -> enabled -> existed?

instance MonadTrackers AppM where
  listTrackers = do
    tv <- trackersTVar
    sortOn trackerId . HM.elems <$> liftIO (readTVarIO tv)

  getTracker tid = do
    tv <- trackersTVar
    liftIO (HM.lookup tid <$> readTVarIO tv)

  addTracker name url key enabled = do
    tid <- liftIO newTrackerId
    let t = Tracker { trackerId = tid, trackerName = name, trackerUrl = url
                    , trackerApiKey = key, trackerEnabled = enabled }
    _ <- modifyTrackers (HM.insert tid t)
    pure t

  deleteTracker tid = do
    m <- getTracker tid
    case m of
      Nothing -> pure False
      Just _  -> do
        _ <- modifyTrackers (HM.delete tid)
        pure True

  setTrackerEnabled tid enabled = do
    m <- getTracker tid
    case m of
      Nothing -> pure False
      Just t  -> do
        _ <- modifyTrackers (HM.insert tid (t { trackerEnabled = enabled }))
        pure True

-- | Module init: load the persisted @trackers.json@ into a TVar under
-- 'trackersStateKey'. On FIRST boot (file absent) with FMS_JACKETT_URL +
-- FMS_JACKETT_API_KEY both set, seed one enabled "jackett" tracker.
loadTrackersIntoState :: AppM ()
loadTrackersIntoState = do
  cfg <- asks envConfig
  liftIO (createDirectoryIfMissing True (configStateDir cfg))
  let path = trackersFilePath cfg
  exists <- liftIO (doesFileExist path)
  loaded <- liftIO (loadTrackers path)
  seeded <-
    if exists || not (isJust (configJackettUrl cfg) && isJust (configJackettApiKey cfg))
      then pure loaded
      else do
        let t = Tracker { trackerId = "jackett"
                        , trackerName = "Jackett"
                        , trackerUrl = maybe "" id (configJackettUrl cfg)
                        , trackerApiKey = maybe "" id (configJackettApiKey cfg)
                        , trackerEnabled = True }
        unless exists (liftIO (saveTrackers path (HM.singleton "jackett" t)))
        pure (HM.singleton "jackett" t)
  unless exists (liftIO (saveTrackers path seeded))  -- seed file on first boot
  tv <- liftIO (newTVarIO seeded)
  putState trackersStateKey tv

-- Internals ---------------------------------------------------------------

trackersTVar :: AppM (TVar (HashMap Text Tracker))
trackersTVar = do
  m <- getState trackersStateKey
  case m of
    Just tv -> pure tv
    Nothing -> do
      tv <- liftIO (newTVarIO HM.empty)
      putState trackersStateKey tv
      pure tv

-- | Apply an update to the trackers TVar AND persist to
-- @stateDir/trackers.json@ after every mutation.
modifyTrackers
  :: (HashMap Text Tracker -> HashMap Text Tracker)
  -> AppM (HashMap Text Tracker)
modifyTrackers f = do
  cfg <- asks envConfig
  tv  <- trackersTVar
  updated <- liftIO $ atomically $ do
    modifyTVar' tv f
    readTVar tv
  liftIO (saveTrackers (trackersFilePath cfg) updated)
  pure updated

-- | A fresh tracker id: 16 random hex chars.
newTrackerId :: IO Text
newTrackerId = do
  g <- newStdGen
  let rs = take 16 (randomRs (0, 15) g) :: [Int]
  pure (T.pack (map (\r -> "0123456789abcdef" !! r) rs))
