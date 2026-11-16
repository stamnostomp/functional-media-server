module FMS.Modules.Collections.Monad
  ( MonadCollections (..)
  , collectionsStateKey
  , collectionsFilePath
  , loadCollectionsIntoState
  , addItem
  , removeItem
  , newCollectionId
  ) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Random (newStdGen, randomRs)

import FMS.Core.Monad (AppM, MonadLog (..), envConfig, getState, putState)
import FMS.Core.Types.Config (Config (..))
import FMS.Core.Types.MediaItem (MediaItem (..))
import FMS.Modules.Library (MonadLibrary (..))
import FMS.Modules.Collections.Persistence (loadCollections, saveCollections)
import FMS.Modules.Collections.Types (Collection (..))

-- | Namespaced key under which the collections module stashes its
-- @TVar (HashMap Text Collection)@ in the typed registry
-- (docs/modules.org: the core never knows module state).
collectionsStateKey :: Text
collectionsStateKey = "collections.map"

-- | Where the collection map is persisted: @stateDir/collections.json@.
collectionsFilePath :: Config -> FilePath
collectionsFilePath cfg = configStateDir cfg </> "collections.json"

-- | Capability for managing collections / box sets (milestone 7). The
-- @Bool@ results of the mutators mean "the collection existed and was
-- changed" — the routes map 'False' to 404.
class Monad m => MonadCollections m where
  listCollections         :: m [Collection]                       -- ^ all collections, oldest first
  getCollection           :: Text -> m (Maybe Collection)         -- ^ by id
  createCollection        :: Text -> [Text] -> m Collection       -- ^ name, item ids (unknown ids dropped)
  deleteCollection        :: Text -> m Bool                       -- ^ id -> existed?
  addItemsToCollection    :: Text -> [Text] -> m Bool             -- ^ collection id, item ids -> changed?
  removeItemFromCollection :: Text -> Text -> m Bool              -- ^ collection id, item id -> changed?

instance MonadCollections AppM where
  listCollections = do
    tv <- collectionsTVar
    sortOn (\c -> (colAddedAt c, colId c)) . HM.elems <$> liftIO (readTVarIO tv)

  getCollection cid = do
    tv <- collectionsTVar
    liftIO (HM.lookup cid <$> readTVarIO tv)

  createCollection name itemIds = do
    -- Unknown item ids are dropped silently (but logged); the valid ones
    -- are kept in the given order.
    allItems <- listItems
    let known = HM.fromList [(itemId i, ()) | i <- allItems]
        valid   = filter (\i -> HM.member i known) itemIds
        dropped = filter (\i -> not (HM.member i known)) itemIds
    unless (null dropped) $
      logInfo ("collections: dropping unknown item id(s) from new collection: "
               <> T.intercalate ", " dropped)
    now <- liftIO getCurrentTime
    cid <- liftIO newCollectionId
    let c = Collection cid name valid now
    _ <- modifyCollections (HM.insert cid c)
    pure c

  deleteCollection cid = do
    mC <- getCollection cid
    case mC of
      Nothing -> pure False
      Just _  -> do
        _ <- modifyCollections (HM.delete cid)
        pure True

  addItemsToCollection cid iids = do
    mC <- getCollection cid
    case mC of
      Nothing -> pure False
      Just c  -> do
        _ <- modifyCollections
               (HM.insert cid (c { colItemIds = foldl (flip addItem) (colItemIds c) iids }))
        pure True

  removeItemFromCollection cid iid = do
    mC <- getCollection cid
    case mC of
      Nothing -> pure False
      Just c  -> do
        _ <- modifyCollections (HM.insert cid (c { colItemIds = removeItem iid (colItemIds c) }))
        pure True

-- | Pure: append @x@ to the item list unless already present (dedupe,
-- order preserved).
addItem :: Text -> [Text] -> [Text]
addItem x xs = if x `elem` xs then xs else xs ++ [x]

-- | Pure: remove every occurrence of @x@ from the item list.
removeItem :: Text -> [Text] -> [Text]
removeItem x = filter (/= x)

-- | A fresh collection id: 16 random hex chars (same minting style as the
-- playlists/sessions/auth ids).
newCollectionId :: IO Text
newCollectionId = do
  g <- newStdGen
  let rs = take 16 (randomRs (0, 15) g) :: [Int]
  pure (T.pack (map (\r -> "0123456789abcdef" !! r) rs))

-- | Module init: load the persisted @collections.json@ into a fresh TVar
-- under 'collectionsStateKey' (creating the state dir if needed).
loadCollectionsIntoState :: AppM ()
loadCollectionsIntoState = do
  cfg <- asks envConfig
  liftIO (createDirectoryIfMissing True (configStateDir cfg))
  loaded <- liftIO (loadCollections (collectionsFilePath cfg))
  tv <- liftIO (newTVarIO loaded)
  putState collectionsStateKey tv

-- Internals ---------------------------------------------------------------

collectionsTVar :: AppM (TVar (HashMap Text Collection))
collectionsTVar = do
  m <- getState collectionsStateKey
  case m of
    Just tv -> pure tv
    Nothing -> do
      tv <- liftIO (newTVarIO (HM.empty :: HashMap Text Collection))
      putState collectionsStateKey tv
      pure tv

-- | Apply an update to the collections TVar AND persist the result to
-- @stateDir/collections.json@ after every mutation (the Stats/Metadata
-- module pattern: in-memory for reads, JSON on disk for restarts).
modifyCollections
  :: (HashMap Text Collection -> HashMap Text Collection)
  -> AppM (HashMap Text Collection)
modifyCollections f = do
  cfg <- asks envConfig
  tv  <- collectionsTVar
  updated <- liftIO $ atomically $ do
    modifyTVar' tv f
    readTVar tv
  liftIO (saveCollections (collectionsFilePath cfg) updated)
  pure updated
