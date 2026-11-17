{-# LANGUAGE CPP #-}

-- | Filesystem watching (docs/todo.org section 2, milestone 2d):
-- when @configWatch@ is set (env @FMS_WATCH=1@), an inotify watcher
-- thread watches every media folder recursively and, after a 2-second
-- debounce window with pending media-file events, runs an incremental
-- 'rescanLibrary' and broadcasts @LibraryChanged@ over the websocket
-- (milestone 8) and the webhooks (milestone 12). hinotify is Linux-only,
-- so the watcher body is compiled only on Linux (CPP); elsewhere
-- 'startWatcher' just logs that watching is disabled.
module FMS.Modules.Library.Watch
  ( startWatcher
  , eventsPending
  , watchMediaFile
  , watchFile
  ) where

import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)

import FMS.Core.Monad (AppM, MonadLog(..), envConfig)
import FMS.Core.Types.Config (Config(..))
import FMS.Modules.Library.Scan (isMediaFile, isNfoFile)

#ifdef FMS_LINUX
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, catch)
import Control.Monad.Reader (ask)
import Data.Aeson (object)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (nub)
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath ((</>))
import System.INotify (Event(..), EventVariety(..), INotify, addWatch, withINotify)
import qualified Data.ByteString.Char8 as BSC

import FMS.Core.Monad (AppEnv, runApp)
import FMS.Core.Types.MediaFolder (MediaFolder(..))
import FMS.Modules.Library.Monad (MonadLibrary(..))
import FMS.Modules.Library.Scan (itemIdsForNfo)
import FMS.Modules.Metadata.Monad (MonadMetadata (..))
import FMS.Modules.Socket (broadcastSocket)
import FMS.Modules.Webhooks.Monad (fireWebhooks)
#endif

-- | Pure: is there at least one event in @evs@ whose timestamp is within
-- @window@ seconds of @now@? Used by the debouncer loop.
eventsPending :: [(UTCTime, FilePath)] -> UTCTime -> NominalDiffTime -> Bool
eventsPending evs now window =
  any (\(t, _) -> diffUTCTime now t <= window) evs

-- | Pure: should a filesystem event for this path trigger a rescan?
-- Only media files (the scanner's extension predicate) count; events
-- for dotfiles/etc. are ignored.
watchMediaFile :: FilePath -> Bool
watchMediaFile = isMediaFile

-- | Pure: should a filesystem event for this path trigger a rescan AND
-- metadata invalidation? Media files yes; NFO sidecars ('.nfo') also count
-- (invalidation audit: a changed sidecar must refresh the sibling item's
-- metadata, so .nfo events are no longer dropped).
watchFile :: FilePath -> Bool
watchFile p = isMediaFile p || isNfoFile p

-- | Entry point called from the library module's init. When
-- 'configWatch' is False this only logs; when True (Linux) it forks the
-- inotify watcher thread; elsewhere it logs that watching is unsupported.
startWatcher :: AppM ()
startWatcher = do
  cfg <- asks envConfig
  if configWatch cfg
    then startWatching cfg
    else logInfo "watch: disabled (set FMS_WATCH=1 to enable inotify auto-rescan)"

#ifdef FMS_LINUX

startWatching :: Config -> AppM ()
startWatching _cfg = do
  folders <- listFolders
  env <- ask
  logInfo ("watch: watching " <> T.pack (show (length folders))
           <> " media folder(s) for changes")
  liftIO $ do
    _ <- forkIO
      (watchLoop env folders `catch` \e -> do
        _ <- runApp env (logErr ("watch: watcher thread died: "
                                 <> T.pack (show (e :: SomeException))))
        pure ())
    pure ()

-- | The watcher thread: one inotify instance, one recursive watch per
-- media folder, then the debounce loop forever.
watchLoop :: AppEnv -> [MediaFolder] -> IO ()
watchLoop env folders = withINotify $ \inotify -> do
  evRef <- newIORef []
  forM_ folders (\f -> watchDirRecursive inotify evRef (folderPath f))
  debounceLoop env evRef

-- | Add a watch on @dir@ and recurse into its subdirectories (hinotify
-- watches are per-directory, not recursive by themselves).
watchDirRecursive :: INotify -> IORef [(UTCTime, FilePath)] -> FilePath -> IO ()
watchDirRecursive inotify evRef dir = do
  ok <- doesDirectoryExist dir
  if not ok
    then pure ()
    else do
      _ <- addWatch inotify [Create, Delete, Modify, Move] (BSC.pack dir) (onEvent evRef)
      entries <- listDirectory dir
      forM_ entries $ \e -> do
        let p = dir </> e
        isDir <- doesDirectoryExist p
        when isDir (watchDirRecursive inotify evRef p)

-- | Record media-file and NFO-sidecar events (timestamped) into the
-- IORef for the debouncer; everything else is dropped.
onEvent :: IORef [(UTCTime, FilePath)] -> Event -> IO ()
onEvent evRef ev = do
  mp <- eventFile ev
  case mp of
    Just p | watchFile p -> do
      now <- getCurrentTime
      modifyIORef' evRef ((now, p) :)
    _ -> pure ()

-- | Extract the affected path from a hinotify 0.4 'Event' (constructors
-- carry it as @maybeFilePath@ or @filePath@ depending on the variety),
-- decoding the RawFilePath bytes back to a 'FilePath'.
-- (hinotify's RawFilePath is a ByteString; Char8 pack/unpack is fine
-- here because the paths are only used for the media-extension check
-- and ASCII log lines.)
eventFile :: Event -> IO (Maybe FilePath)
eventFile e = pure $ case e of
  Accessed _ p   -> BSC.unpack <$> p
  Modified _ p   -> BSC.unpack <$> p
  Attributes _ p -> BSC.unpack <$> p
  Closed _ p _   -> BSC.unpack <$> p
  Opened _ p     -> BSC.unpack <$> p
  MovedOut _ p _ -> Just (BSC.unpack p)
  MovedIn _ p _  -> Just (BSC.unpack p)
  Created _ p    -> Just (BSC.unpack p)
  Deleted _ p    -> Just (BSC.unpack p)
  _              -> Nothing

-- | Every 2 seconds: if any media-file event arrived during the window,
-- rescan incrementally and broadcast LibraryChanged (socket + webhooks).
debounceLoop :: AppEnv -> IORef [(UTCTime, FilePath)] -> IO ()
debounceLoop env evRef = do
  threadDelay 2000000
  now <- getCurrentTime
  evs <- readIORef evRef
  writeIORef evRef []
  if eventsPending evs now 2.0
    then do
      r <- runApp env $ do
        logInfo ("watch: " <> T.pack (show (length evs))
                 <> " change(s) detected, rescanning")
        rescanLibrary
        -- Invalidation audit: an NFO sidecar change does not alter any
        -- media file's size/mtime, so the rescan diff alone would not
        -- notice it -- explicitly refresh the metadata of the sibling
        -- item(s) (movie.nfo -> the movie; tvshow.nfo -> its episodes).
        items <- listItems
        let nfoPaths = [ p | (_, p) <- evs, isNfoFile p ]
            ids      = nub (concatMap (itemIdsForNfo items) nfoPaths)
        forM_ ids $ \i -> do
          _ <- refreshMetadata i
          pure ()
        broadcastSocket "LibraryChanged" (object [])
        fireWebhooks "LibraryChanged" (object [])
      case r of
        Left err -> do
          _ <- runApp env (logErr ("watch: rescan failed: " <> T.pack (show err)))
          pure ()
        Right () -> pure ()
    else pure ()
  debounceLoop env evRef

#else

startWatching :: Config -> AppM ()
startWatching _cfg =
  logInfo "watch: inotify watching is Linux-only; disabled on this platform"

#endif
