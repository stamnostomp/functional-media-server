module FMS.Modules.Library.Monad
  ( MonadLibrary(..)
  , libraryAcidKey
  , libraryFoldersKey
  , initFolders
  , withScanLock
  , scanLockStaleSeconds
  ) where

import Control.Concurrent.STM
  ( TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO )
import Control.Exception (SomeException, bracket_, catch, throwIO, try)
import Control.Monad (forM, forM_)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Acid (AcidState, query, update)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Directory
  ( createDirectory
  , createDirectoryIfMissing
  , getFileSize
  , getModificationTime
  , removeDirectory
  , removeDirectoryRecursive
  )
import System.FilePath ((</>))

import FMS.Core.Monad (AppError(..), AppM, MonadLog(..), envConfig, getState, putState)
import FMS.Core.Types.Config (Config(..))
import FMS.Core.Types.MediaFolder (MediaFolder(..))
import FMS.Core.Types.MediaItem (MediaItem(..))
import FMS.Modules.Library.Folders
  ( defaultFolders, foldersFilePath, loadFolders, nextFolderId, saveFolders )
import FMS.Modules.Library.Scan
  ( isMediaFile, itemFromPath, planRescan, walkWithLoops )
import FMS.Modules.Library.State
  ( LibraryState
  , GetAllItems(..)
  , GetIdByPath(..)
  , GetItemById(..)
  , RegisterItem(..)
  , RemovePaths(..)
  , UpdateItemMeta(..)
  )

-- | Namespaced key under which the module stashes its 'AcidState' in the
-- typed registry (docs/modules.org: the core never knows module state).
libraryAcidKey :: Text
libraryAcidKey = "library.acid"

-- | Registry key for the media folders:
-- @TVar (HashMap Text MediaFolder)@, persisted to
-- @stateDir/folders.json@ on every change (milestone 2c).
libraryFoldersKey :: Text
libraryFoldersKey = "library.folders"

-- | The library capability (docs/modules.org). Other modules can require
-- @MonadLibrary m => ...@ in their handlers.
class Monad m => MonadLibrary m where
  listItems     :: m [MediaItem]
  getItem       :: Text -> m (Maybe MediaItem)
  rescanLibrary :: m ()
  listFolders   :: m [MediaFolder]                        -- ^ milestone 2c
  addFolder     :: Text -> FilePath -> Text -> m MediaFolder  -- ^ name, path, type
  removeFolder  :: Text -> m Bool                         -- ^ False when the id is unknown

-- | Run an acid-state action with the 'AcidState' stored in the registry by
-- the module init.
withAcid :: (AcidState LibraryState -> IO a) -> AppM a
withAcid f = do
  mAcid <- getState libraryAcidKey
  case mAcid of
    Nothing   -> throwError (InternalError (libraryAcidKey <> " not initialised"))
    Just acid -> liftIO (f acid)

instance MonadLibrary AppM where
  listItems = withAcid (`query` GetAllItems)

  getItem i = withAcid (`query` GetItemById i)

  listFolders = do
    tv <- foldersTVar
    sortOn folderId . HM.elems <$> liftIO (readTVarIO tv)

  addFolder name path typ = do
    liftIO (createDirectoryIfMissing True path)
    tv <- foldersTVar
    fid <- liftIO $ atomically $ do
      m <- readTVar tv
      pure (nextFolderId m)
    let f = MediaFolder { folderId = fid, folderName = name
                        , folderPath = path, folderType = typ }
    _ <- modifyFolders (HM.insert fid f)
    pure f

  removeFolder fid = do
    tv  <- foldersTVar
    m   <- liftIO (readTVarIO tv)
    case HM.lookup fid m of
      Nothing -> pure False
      Just _  -> do
        _ <- modifyFolders (HM.delete fid)
        -- also drop the folder's items from acid-state (their paths will
        -- not be in any future scan union either)
        items <- withAcid (`query` GetAllItems)
        let paths = [ itemPath i | i <- items, itemFolder i == fid ]
        _ <- withAcid (`update` RemovePaths paths)
        pure True

  -- | Incremental rescan (milestone 2b, docs/todo.org section 2), extended
  -- in milestone 2c to walk EVERY media folder: under the scan lock, walk
  -- each folder (symlink policy from config), stat every media file and
  -- tag it with its folder id, then diff the union against the registered
  -- paths ('planRescan'): remove gone paths (including paths of removed
  -- folders), register new ones, touch changed ones -- unchanged files
  -- keep both their id and their stored record.
  rescanLibrary = do
    cfg <- asks envConfig
    let follow  = configFollowSymlinks cfg
        lockDir = configStateDir cfg </> "scan.lock"
    folders <- listFolders
    (scannedStats, folderOf, loops) <- liftIO $ withScanLock lockDir $ do
      perFolder <- forM folders $ \f -> do
        (files, loops) <- walkWithLoops follow (folderPath f)
        stats <- forM (filter isMediaFile files) $ \p -> do
          s <- getFileSize p
          t <- getModificationTime p
          pure (p, s, t)
        pure (stats, loops, folderId f)
      let scannedStats = concat [ ss | (ss, _, _) <- perFolder ]
          allLoops     = concat [ ls | (_, ls, _) <- perFolder ]
          folderOf     = Map.fromList
            [ (p, fid) | (ss, _, fid) <- perFolder, (p, _, _) <- ss ]
      pure (scannedStats, folderOf, allLoops)
    forM_ loops $ \l ->
      logInfo ("library: skipping circular symlink " <> T.pack l)
    existingItems <- withAcid (`query` GetAllItems)
    let existingMeta = [ (itemPath i, Just (itemSize i, itemAddedAt i)) | i <- existingItems ]
        scannedMap   = Map.fromList [ (p, (s, t)) | (p, s, t) <- scannedStats ]
        (removed, changedOrNew, _unchanged) = planRescan scannedStats existingMeta
    removedCount <- withAcid (`update` RemovePaths removed)
    applied <- forM changedOrNew $ \p -> do
      let (s, t) = scannedMap Map.! p
      mId <- withAcid (`query` GetIdByPath p)
      case mId of
        Just _ -> do
          changed <- withAcid (`update` UpdateItemMeta p s t)
          pure (if changed then (1 :: Int) else 0)
        Nothing -> do
          item <- liftIO (itemFromPath (folderOf Map.! p) p)
          _ <- withAcid (`update` RegisterItem p item)
          pure 1
    logInfo (T.pack (unwords
      [ "library rescan:"
      , show (length applied), "added/changed,"
      , show removedCount, "removed,"
      , show (length scannedStats), "media file(s) found in"
      , show (length folders), "folder(s)"
      ]))

-- | Module init part: load @stateDir/folders.json@ (or the single default
-- folder rooted at the configured library path), persist it back (so the
-- file exists from first boot), and stash the map in the registry.
initFolders :: AppM ()
initFolders = do
  cfg <- asks envConfig
  liftIO (createDirectoryIfMissing True (configStateDir cfg))
  loaded <- liftIO (loadFolders (foldersFilePath cfg))
  let folders = if HM.null loaded then defaultFolders cfg else loaded
  liftIO (saveFolders (foldersFilePath cfg) folders)
  tv <- liftIO (newTVarIO folders)
  putState libraryFoldersKey tv

-- | Update the folders TVar AND persist to @folders.json@ after every
-- change; returns the updated map.
modifyFolders
  :: (HashMap Text MediaFolder -> HashMap Text MediaFolder) -> AppM (HashMap Text MediaFolder)
modifyFolders f = do
  cfg <- asks envConfig
  tv  <- foldersTVar
  updated <- liftIO $ atomically $ do
    modifyTVar' tv f
    readTVar tv
  liftIO (saveFolders (foldersFilePath cfg) updated)
  pure updated

-- Internals ---------------------------------------------------------------

foldersTVar :: AppM (TVar (HashMap Text MediaFolder))
foldersTVar = do
  m <- getState libraryFoldersKey
  case m of
    Just tv -> pure tv
    Nothing -> do
      cfg <- asks envConfig
      loaded <- liftIO (loadFolders (foldersFilePath cfg))
      let folders = if HM.null loaded then defaultFolders cfg else loaded
      tv <- liftIO (newTVarIO folders)
      putState libraryFoldersKey tv
      pure tv

-- | Scan lock (milestone 2b): an atomic lock directory. Acquiring is
-- 'createDirectory' (atomic: fails when it already exists); a lock older
-- than 'scanLockStaleSeconds' is considered stale (crashed holder) and is
-- removed and re-acquired once. Releasing removes the (empty) directory.
-- Bracket-style: the lock is released even when the action throws.
withScanLock :: FilePath -> IO a -> IO a
withScanLock lockDir action =
  bracket_ (acquireScanLock lockDir) (releaseScanLock lockDir) action

-- | A scan lock older than this many seconds is stale and recovered.
scanLockStaleSeconds :: Double
scanLockStaleSeconds = 60

acquireScanLock :: FilePath -> IO ()
acquireScanLock lockDir = do
  r <- try (createDirectory lockDir) :: IO (Either SomeException ())
  case r of
    Right () -> pure ()
    Left _ -> do
      now   <- getCurrentTime
      mtime <- getModificationTime lockDir
      if realToFrac (diffUTCTime now mtime) > scanLockStaleSeconds
        then do
          -- stale lock (holder crashed): remove and retry once
          removeDirectoryRecursive lockDir `catch` \(_ :: SomeException) -> pure ()
          r2 <- try (createDirectory lockDir) :: IO (Either SomeException ())
          case r2 of
            Right () -> pure ()
            Left e   -> throwIO e
        else throwIO (userError ("scan lock is held: " ++ lockDir))

releaseScanLock :: FilePath -> IO ()
releaseScanLock lockDir =
  removeDirectory lockDir `catch` \(_ :: SomeException) -> pure ()
