module FMS.Modules.Library.Monad
  ( MonadLibrary(..)
  , libraryAcidKey
  , withScanLock
  , scanLockStaleSeconds
  ) where

import Control.Exception (SomeException, bracket_, catch, throwIO, try)
import Control.Monad (forM, forM_)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import qualified Data.Map.Strict as Map
import Data.Acid (AcidState, query, update)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Directory
  ( createDirectory
  , getFileSize
  , getModificationTime
  , removeDirectory
  , removeDirectoryRecursive
  )
import System.FilePath ((</>))

import FMS.Core.Monad (AppError(..), AppM, MonadLog(..), envConfig, getState)
import FMS.Core.Types.Config (Config(..))
import FMS.Core.Types.MediaItem (MediaItem(..))
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

-- | The library capability (docs/modules.org). Other modules can require
-- @MonadLibrary m => ...@ in their handlers.
class Monad m => MonadLibrary m where
  listItems     :: m [MediaItem]
  getItem       :: Text -> m (Maybe MediaItem)
  rescanLibrary :: m ()

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

  -- | Incremental rescan (milestone 2b, docs/todo.org section 2): walk the
  -- library under the scan lock (symlink policy from config), stat every
  -- media file, diff against the registered paths ('planRescan'), then
  -- remove gone paths, register new ones, and touch changed ones --
  -- unchanged files keep both their id and their stored record.
  rescanLibrary = do
    cfg <- asks envConfig
    let follow  = configFollowSymlinks cfg
        lockDir = configStateDir cfg </> "scan.lock"
    (scannedStats, loops) <- liftIO $ withScanLock lockDir $ do
      (files, loops) <- walkWithLoops follow (configLibraryPath cfg)
      let mediaFiles = filter isMediaFile files
      stats <- forM mediaFiles $ \p -> do
        s <- getFileSize p
        t <- getModificationTime p
        pure (p, s, t)
      pure (stats, loops)
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
          item <- liftIO (itemFromPath p)
          _ <- withAcid (`update` RegisterItem p item)
          pure 1
    logInfo (T.pack (unwords
      [ "library rescan:"
      , show (length applied), "added/changed,"
      , show removedCount, "removed,"
      , show (length scannedStats), "media file(s) found in"
      , configLibraryPath cfg
      ]))

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
