module FMS.Modules.Segments.Monad
  ( MonadSegments (..)
  , segmentsStateKey
  , segmentsFilePath
  , loadSegmentsIntoState
  , newSegmentId
  ) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Random (newStdGen, randomRs)

import FMS.Core.Monad (AppM, envConfig, getState, putState)
import FMS.Core.Types.Config (Config (..))
import FMS.Modules.Segments.Persistence (loadSegments, saveSegments)
import FMS.Modules.Segments.Types (MediaSegment (..))

-- | Namespaced key under which the segments module stashes its
-- @TVar (HashMap Text [MediaSegment])@ (item id -> segments) in the
-- typed registry (docs/modules.org: the core never knows module state).
segmentsStateKey :: Text
segmentsStateKey = "segments.map"

-- | Where the segment map is persisted: @stateDir/mediaSegments.json@.
segmentsFilePath :: Config -> FilePath
segmentsFilePath cfg = configStateDir cfg </> "mediaSegments.json"

-- | Capability for managing media segments (milestone 6h). Mutators
-- return 'True' when the change was applied.
class Monad m => MonadSegments m where
  listSegments    :: Text -> m [MediaSegment]            -- ^ by item id, sorted by start
  upsertSegment   :: MediaSegment -> m MediaSegment      -- ^ insert or replace by segId
  deleteSegment   :: Text -> Text -> m Bool              -- ^ item id, segment id -> existed?

instance MonadSegments AppM where
  listSegments iid = do
    tv <- segmentsTVar
    segs <- liftIO (HM.lookup iid <$> readTVarIO tv)
    pure (sortOn segStartTicks (maybe [] id segs))

  upsertSegment seg = do
    -- HM.alter (not HM.adjust): the item key may not exist yet
    _ <- modifySegments
           (HM.alter (Just . upsert (segId seg) . fromMaybe []) (segItemId seg))
    pure seg
    where
      upsert sid ss =
        if any ((== sid) . segId) ss
          then filter ((/= sid) . segId) ss ++ [seg]
          else ss ++ [seg]

  deleteSegment iid sid = do
    mSegs <- listSegments iid
    if not (any ((== sid) . segId) mSegs)
      then pure False
      else do
        _ <- modifySegments (HM.adjust (filter ((/= sid) . segId)) iid)
        pure True  -- key exists here (listSegments found the segment)

-- | A fresh segment id: 16 random hex chars (same minting style as the
-- collections/auth ids).
newSegmentId :: IO Text
newSegmentId = do
  g <- newStdGen
  let rs = take 16 (randomRs (0, 15) g) :: [Int]
  pure (T.pack (map (\r -> "0123456789abcdef" !! r) rs))

-- | Module init: load the persisted @mediaSegments.json@ into a fresh
-- TVar under 'segmentsStateKey' (creating the state dir if needed).
loadSegmentsIntoState :: AppM ()
loadSegmentsIntoState = do
  cfg <- asks envConfig
  liftIO (createDirectoryIfMissing True (configStateDir cfg))
  loaded <- liftIO (loadSegments (segmentsFilePath cfg))
  tv <- liftIO (newTVarIO loaded)
  putState segmentsStateKey tv

-- Internals ---------------------------------------------------------------

segmentsTVar :: AppM (TVar (HashMap Text [MediaSegment]))
segmentsTVar = do
  m <- getState segmentsStateKey
  case m of
    Just tv -> pure tv
    Nothing -> do
      tv <- liftIO (newTVarIO (HM.empty :: HashMap Text [MediaSegment]))
      putState segmentsStateKey tv
      pure tv

-- | Apply an update to the segments TVar AND persist the result to
-- @stateDir/mediaSegments.json@ after every mutation (the
-- Collections/Stats pattern).
modifySegments
  :: (HashMap Text [MediaSegment] -> HashMap Text [MediaSegment])
  -> AppM (HashMap Text [MediaSegment])
modifySegments f = do
  cfg <- asks envConfig
  tv  <- segmentsTVar
  updated <- liftIO $ atomically $ do
    modifyTVar' tv f
    readTVar tv
  liftIO (saveSegments (segmentsFilePath cfg) updated)
  pure updated
