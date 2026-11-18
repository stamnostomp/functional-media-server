module FMS.Modules.Indexers.Monad
  ( MonadIndexers (..)
  , indexersStateKey
  , indexersFilePath
  , loadIndexersIntoState
  , newIndexerId
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
import Data.Time.Clock (getCurrentTime)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.Random (newStdGen, randomRs)

import FMS.Core.Monad (AppM, envConfig, getState, putState)
import FMS.Core.Types.Config (Config (..))
import FMS.Modules.Indexers.Persistence (loadIndexers, saveIndexers)
import FMS.Modules.Indexers.Types (Indexer (..), defaultCategories)

-- | Namespaced key under which the indexers module stashes its
-- @TVar (HashMap Text Indexer)@ in the typed registry.
indexersStateKey :: Text
indexersStateKey = "indexers.map"

-- | Where the indexer map is persisted: @stateDir/indexers.json@.
indexersFilePath :: Config -> FilePath
indexersFilePath cfg = configStateDir cfg </> "indexers.json"

-- | Capability for the direct-indexer registry (m21).
class Monad m => MonadIndexers m where
  listIndexers          :: m [Indexer]                  -- ^ all, sorted by Id
  getIndexer            :: Text -> m (Maybe Indexer)    -- ^ by id
  insertIndexer         :: Indexer -> m Indexer         -- ^ persisted insert
  deleteIndexer         :: Text -> m Bool               -- ^ id -> existed?
  setIndexerEnabled     :: Text -> Bool -> m Bool       -- ^ id -> enabled -> existed?
  setIndexerTestResult  :: Text -> Bool -> m Bool
    -- ^ id -> ok; bumps LastTestAt/LastTestOk, persisted

instance MonadIndexers AppM where
  listIndexers = do
    tv <- indexersTVar
    sortOn indexerId . HM.elems <$> liftIO (readTVarIO tv)

  getIndexer iid = do
    tv <- indexersTVar
    liftIO (HM.lookup iid <$> readTVarIO tv)

  insertIndexer i = do
    _ <- modifyIndexers (HM.insert (indexerId i) i)
    pure i

  deleteIndexer iid = do
    m <- getIndexer iid
    case m of
      Nothing -> pure False
      Just _  -> do
        _ <- modifyIndexers (HM.delete iid)
        pure True

  setIndexerEnabled iid enabled = do
    m <- getIndexer iid
    case m of
      Nothing -> pure False
      Just i  -> do
        _ <- modifyIndexers (HM.insert iid (i { indexerEnabled = enabled }))
        pure True

  setIndexerTestResult iid ok = do
    now <- liftIO getCurrentTime
    m <- getIndexer iid
    case m of
      Nothing -> pure False
      Just i  -> do
        _ <- modifyIndexers (HM.insert iid (i { indexerLastTestAt = Just now
                                              , indexerLastTestOk = Just ok }))
        pure True

-- | Module init: load the persisted @indexers.json@ into a TVar under
-- 'indexersStateKey'. On FIRST boot (file absent) with FMS_INDEXER_URL +
-- FMS_INDEXER_API_KEY both set, seed one enabled direct indexer with id
-- "seed" (name from FMS_INDEXER_NAME, default "seed").
loadIndexersIntoState :: AppM ()
loadIndexersIntoState = do
  cfg <- asks envConfig
  liftIO (createDirectoryIfMissing True (configStateDir cfg))
  let path = indexersFilePath cfg
  exists <- liftIO (doesFileExist path)
  loaded <- liftIO (loadIndexers path)
  seeded <-
    if exists || not (isJust (configIndexerUrl cfg) && isJust (configIndexerApiKey cfg))
      then pure loaded
      else do
        let i = Indexer { indexerId = "seed"
                        , indexerName = maybe "seed" id (configIndexerName cfg)
                        , indexerBaseUrl = maybe "" id (configIndexerUrl cfg)
                        , indexerApiKey = maybe "" id (configIndexerApiKey cfg)
                        , indexerCategories = defaultCategories
                        , indexerType = "torznab"
                        , indexerEnabled = True
                        , indexerLastTestAt = Nothing
                        , indexerLastTestOk = Nothing }
        unless exists (liftIO (saveIndexers path (HM.singleton "seed" i)))
        pure (HM.singleton "seed" i)
  unless exists (liftIO (saveIndexers path seeded))  -- seed file on first boot
  tv <- liftIO (newTVarIO seeded)
  putState indexersStateKey tv

-- Internals ---------------------------------------------------------------

indexersTVar :: AppM (TVar (HashMap Text Indexer))
indexersTVar = do
  m <- getState indexersStateKey
  case m of
    Just tv -> pure tv
    Nothing -> do
      tv <- liftIO (newTVarIO HM.empty)
      putState indexersStateKey tv
      pure tv

-- | Apply an update to the indexers TVar AND persist to
-- @stateDir/indexers.json@ after every mutation.
modifyIndexers
  :: (HashMap Text Indexer -> HashMap Text Indexer)
  -> AppM (HashMap Text Indexer)
modifyIndexers f = do
  cfg <- asks envConfig
  tv  <- indexersTVar
  updated <- liftIO $ atomically $ do
    modifyTVar' tv f
    readTVar tv
  liftIO (saveIndexers (indexersFilePath cfg) updated)
  pure updated

-- | A fresh indexer id: 16 random hex chars.
newIndexerId :: IO Text
newIndexerId = do
  g <- newStdGen
  let rs = take 16 (randomRs (0, 15) g) :: [Int]
  pure (T.pack (map (\r -> "0123456789abcdef" !! r) rs))
