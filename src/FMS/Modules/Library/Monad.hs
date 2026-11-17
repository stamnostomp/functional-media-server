module FMS.Modules.Library.Monad
  ( MonadLibrary(..)
  , libraryAcidKey
  , libraryFoldersKey
  , initFolders
  , withScanLock
  , scanLockStaleSeconds
  , scanProgressData
  ) where

import Control.Concurrent.STM
  ( TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO )
import Control.Exception (SomeException, bracket_, catch, throwIO, try)
import Control.Monad (foldM, forM, forM_)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, asks)
import Data.Either (partitionEithers)
import qualified Data.HashMap.Strict as HM
import Data.HashMap.Strict (HashMap)
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe)
import Data.Acid (AcidState, query, update)
import Data.Aeson (ToJSON (..), Value, object, (.=))
import qualified Data.Aeson.Key as Key
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

import FMS.Core.Monad (AppError(..), AppM, MonadLog(..), envConfig, getState, putState, runApp)
import FMS.Core.Types.Config (Config(..))
import FMS.Core.Types.MediaFolder (MediaFolder(..))
import FMS.Core.Types.MediaItem (MediaItem(..))
import FMS.Modules.Library.Folders
  ( defaultFolders, foldersFilePath, loadFolders, nextFolderId, saveFolders )
import FMS.Modules.Library.Scan
  ( groupVersions, isMediaFile, itemFromGroup, itemFromPath, mimeFor, planRescan
  , walkWithLoops
  )
import FMS.Modules.Library.State
  ( LibraryState
  , GetAllItems(..)
  , GetIdByPath(..)
  , GetItemById(..)
  , RegisterItem(..)
  , RemovePaths(..)
  , RekeyItem(..)
  , UpdateItemMeta(..)
  )
import FMS.Modules.Socket (broadcastSocket)
import FMS.Modules.Webhooks.Monad (fireWebhooks)

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
    env <- ask
    let follow  = configFollowSymlinks cfg
        lockDir = configStateDir cfg </> "scan.lock"
    folders <- listFolders
    -- Milestone 11c (docs/todo.org section 2): scan progress events.
    -- The walk+diff runs under the scan lock in IO (so the lock's bracket
    -- still covers everything), but via 'runApp env' so the per-folder
    -- body below can broadcast/fire events from AppM. "Scanning" is sent
    -- before EACH folder is walked; "Completed"/"Failed" once at the end.
    eres <- liftIO (withScanLock lockDir (runApp env (scanFoldersAndApply folders follow)))
    case eres of
      Left e -> do
        emitScanProgress "Failed" [("Error", toJSON (T.pack (show e)))]
        throwError e
      Right (loops, added, changed, removedCount, promoted, scannedCount) -> do
        forM_ loops $ \l ->
          logInfo ("library: skipping circular symlink " <> T.pack l)
        logInfo (T.pack (unwords
          [ "library rescan:"
          , show added, "added,", show changed, "changed,"
          , show removedCount, "removed,"
          , show (length promoted), "version-promoted,"
          , show scannedCount, "media file(s) found in"
          , show (length folders), "folder(s)"
          ]))
        emitScanProgress "Completed"
          [ ("Added",   toJSON added)
          , ("Changed", toJSON changed)
          , ("Removed", toJSON removedCount)
          ]
        pure ()

-- | The body of 'rescanLibrary' (milestone 11c): walk every folder (after
-- emitting a "Scanning" progress event for it), then run the incremental
-- diff under the scan lock. Returns the loops seen plus the counters for
-- the "Completed" event.
scanFoldersAndApply
  :: [MediaFolder] -> Bool -> AppM ([FilePath], Int, Int, Int, [FilePath], Int)
scanFoldersAndApply folders follow = do
    let isVideo p = "video" `T.isPrefixOf` mimeFor p
    perFolder <- forM folders $ \f -> do
      emitScanProgress "Scanning"
        [ ("FolderId", toJSON (folderId f))
        , ("Folder",   toJSON (folderName f))
        ]
      (files, loops) <- liftIO (walkWithLoops follow (folderPath f))
      stats <- forM (filter isMediaFile files) $ \p -> liftIO $ do
        s <- getFileSize p
        t <- getModificationTime p
        pure (p, s, t)
      pure (stats, loops, folderId f)
    let scannedStats = concat [ ss | (ss, _, _) <- perFolder ]
        allLoops     = concat [ ls | (_, ls, _) <- perFolder ]
        folderOf     = Map.fromList
          [ (p, fid) | (ss, _, fid) <- perFolder, (p, _, _) <- ss ]
    -- Milestone 4j (multi-version media): video files are grouped by
    -- (directory, normalizeVersionTitle basename); each group becomes ONE
    -- item keyed by its best path (the group's primary). Non-video files
    -- stay singletons. The diff below runs over group primaries, so a
    -- version file appearing/disappearing does not create/remove an item --
    -- only a disappearing PRIMARY does, and then the next surviving
    -- version is promoted (same item id) when possible.
    let scannedMap  = Map.fromList [ (p, (s, t)) | (p, s, t) <- scannedStats ]
        videoGroups = groupVersions [ (p, s) | (p, s, _) <- scannedStats, isVideo p ]
        groupPaths  g = map fst g                       -- best first
        primaryOf   g = case g of ((p, _) : _) -> p; [] -> ""  -- groups are non-empty
        nonVideo    = [ (p, s, t) | (p, s, t) <- scannedStats, not (isVideo p) ]
        primaryStats = [ (primaryOf g, s, t) | g <- videoGroups
                       , let (s, t) = scannedMap Map.! primaryOf g ]
        scanList    = primaryStats ++ nonVideo          -- diff keyed by primary
        pathToGroup = Map.fromList [ (p, g) | g <- videoGroups, (p, _) <- g ]
        primarySet  = Map.fromList [ (p, ()) | (p, _, _) <- scanList ]
    existingItems0 <- withAcid (`query` GetAllItems)
    -- promotion pass: a removed primary whose group still has surviving
    -- files is re-keyed to the group's new primary (same item id)
    promotedOrRemoved <- forM [ itemPath i | i <- existingItems0
                               , not (Map.member (itemPath i) primarySet) ] $ \p -> do
      mId <- withAcid (`query` GetIdByPath p)
      mItem <- case mId of
        Nothing -> pure Nothing
        Just i  -> withAcid (`query` GetItemById i)
      case mItem of
        Just item
          | Just g <- listToMaybe [ g | v <- itemVersions item, v /= p
                                       , Just g <- [Map.lookup v pathToGroup] ] -> do
              let vers = if length (groupPaths g) > 1 then groupPaths g else []
              _ <- withAcid (`update` RekeyItem p (primaryOf g) vers)
              pure (Left p)   -- promoted
        _ -> pure (Right p) -- really gone
    let (promoted, reallyRemoved) = partitionEithers promotedOrRemoved
    removedCount <- withAcid (`update` RemovePaths reallyRemoved)
    existingItems <- withAcid (`query` GetAllItems)   -- after promotion/removal
    let existingMeta = [ (itemPath i, Just (itemSize i, itemAddedAt i)) | i <- existingItems ]
        (_removed2, changedOrNew, _unchanged) = planRescan scanList existingMeta
    (added, changed) <- foldM (\(a, c) p -> do
      let (s, t) = scannedMap Map.! p
      mId <- withAcid (`query` GetIdByPath p)
      case mId of
        Just _ -> do
          ch <- withAcid (`update` UpdateItemMeta p s t)
          pure (a, c + (if ch then 1 else 0))
        Nothing -> do
          case Map.lookup p pathToGroup of
            Just g -> do
              item <- liftIO (itemFromGroup (folderOf Map.! p) (groupPaths g))
              _ <- withAcid (`update` RegisterItem p item)
              pure (a + 1, c)
            Nothing -> do
              item <- liftIO (itemFromPath (folderOf Map.! p) p)
              _ <- withAcid (`update` RegisterItem p item)
              pure (a + 1, c)) (0, 0) changedOrNew
    -- refresh the version list of every surviving group whose primary is
    -- already registered (covers unchanged primaries whose membership
    -- changed)
    _ <- forM videoGroups $ \g -> do
      let p = primaryOf g
      mId <- withAcid (`query` GetIdByPath p)
      case mId of
        Just _  -> do
          -- singleton groups carry no version list (itemVersions = [])
          let vers = if length (groupPaths g) > 1 then groupPaths g else []
          _ <- withAcid (`update` RekeyItem p p vers)
          pure ()
        Nothing -> pure ()
    pure (allLoops, added, changed, removedCount, promoted, length scannedStats)

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

-- Milestone 11c (docs/todo.org section 2): scan progress events.

-- | Pure: the @Data@ payload of a @ScanProgress@ socket/webhook event --
-- @{"Status": status, ...fields}@ (the socket layer adds @MessageType@,
-- the webhook layer adds @Event@/@Timestamp@/@ServerId@).
scanProgressData :: Text -> [(Text, Value)] -> Value
scanProgressData status fields =
  object (("Status" .= status) : [ (Key.fromText k, v) | (k, v) <- fields ])

-- | Emit one ScanProgress event to every connected socket client AND to
-- every matching webhook. Both sinks are registry lookups that no-op when
-- their module is not loaded, so this never crashes the scan.
emitScanProgress :: Text -> [(Text, Value)] -> AppM ()
emitScanProgress status fields = do
  let dat = scanProgressData status fields
  broadcastSocket "ScanProgress" dat
  fireWebhooks "ScanProgress" dat
