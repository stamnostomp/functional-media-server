module FMS.Modules.Library.Scan
  ( scanDirectory
  , walk
  , walkWithLoops
  , isCircular
  , diffScan
  , planRescan
  , itemFromPath
  , isMediaFile
  , mimeFor
  , mediaExtensions
  ) where

import Control.Monad (foldM, forM)
import Data.Char (toLower)
import Data.Either (partitionEithers)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import System.Directory
  ( canonicalizePath
  , doesDirectoryExist
  , doesFileExist
  , getFileSize
  , getModificationTime
  , listDirectory
  , pathIsSymbolicLink
  )
import System.FilePath (takeBaseName, takeExtension, (</>))

import FMS.Core.Types.MediaItem (MediaItem(..))

-- | Extensions recognised as media files (lowercase, with dot).
mediaExtensions :: [String]
mediaExtensions =
  [ ".mp4", ".mkv", ".avi", ".webm", ".mov"
  , ".mp3", ".flac", ".wav", ".ogg", ".m4a"
  ]

-- | Is this path a media file, judging by extension?
isMediaFile :: FilePath -> Bool
isMediaFile p = map toLower (takeExtension p) `elem` mediaExtensions

-- | Mime type for a path, from its extension (docs/modules.org scanner).
mimeFor :: FilePath -> Text
mimeFor p = case map toLower (takeExtension p) of
  ".mp4"  -> "video/mp4"
  ".mkv"  -> "video/x-matroska"
  ".avi"  -> "video/x-msvideo"
  ".webm" -> "video/webm"
  ".mov"  -> "video/quicktime"
  ".mp3"  -> "audio/mpeg"
  ".flac" -> "audio/flac"
  ".wav"  -> "audio/wav"
  ".ogg"  -> "audio/ogg"
  ".m4a"  -> "audio/mp4"
  _       -> "application/octet-stream"

-- | Build a 'MediaItem' from a path: size + mtime from the filesystem,
-- title = base name, mime from the extension. The id is left empty ("")
-- -- the acid-state 'RegisterItem' event assigns the stable id.
itemFromPath :: FilePath -> IO MediaItem
itemFromPath p = do
  size  <- getFileSize p
  mtime <- getModificationTime p
  pure MediaItem
    { itemId      = ""
    , itemTitle   = T.pack (takeBaseName p)
    , itemPath    = p
    , itemSize    = size
    , itemMime    = mimeFor p
    , itemAddedAt = mtime
    }

-- | Pure: is @canon@ already in the visited set? Used to detect circular
-- symlink chains while walking (milestone 2b).
isCircular :: Set FilePath -> FilePath -> Bool
isCircular visited canon = Set.member canon visited

-- | Pure: diff an existing path list against a freshly scanned one ->
-- @(toRemove, toScan)@ = @(existing \\ scanned, scanned \\ existing)@.
diffScan :: [FilePath] -> [FilePath] -> ([FilePath], [FilePath])
diffScan existing scanned =
  ( Set.toList (Set.fromList existing `Set.difference` Set.fromList scanned)
  , Set.toList (Set.fromList scanned `Set.difference` Set.fromList existing)
  )

-- | Pure incremental-rescan plan (milestone 2b): given the scanned files
-- with their @(size, mtime)@ and the existing paths with their stored
-- @(size, mtime)@ (Nothing = unknown), return
-- @(removed, changedOrNew, unchanged)@, all sorted.
planRescan
  :: [(FilePath, Integer, UTCTime)]
  -> [(FilePath, Maybe (Integer, UTCTime))]
  -> ([FilePath], [FilePath], [FilePath])
planRescan scanned existing =
  let scannedMap  = Map.fromList [(p, (s, t)) | (p, s, t) <- scanned]
      existingMap = Map.fromList existing
      removed     = sort [ p | (p, _) <- existing, not (Map.member p scannedMap) ]
      classify p  = case Map.lookup p existingMap of
        Nothing         -> Left p  -- new file
        Just Nothing    -> Left p
        Just (Just old) -> if scannedMap Map.! p /= old then Left p else Right p
      (changedOrNew, unchanged) =
        partitionEithers (map classify (sort (Map.keys scannedMap)))
  in (removed, changedOrNew, unchanged)

-- | Walk a directory tree, following symlinks or not, and return
-- @(files, skippedCircularLoops)@. With @followSymlinks = False@ every
-- symlink (to a file or a dir) is skipped. With True, file symlinks are
-- followed (the link path itself is reported) and dir symlinks are
-- descended into; the canonical path of every visited dir is tracked in a
-- set that is threaded through the WHOLE traversal (across siblings), so
-- a symlink pointing at any already-visited dir -- a circular chain or a
-- duplicate of a real dir -- is detected, skipped and reported. Real
-- (non-symlink) entries are walked before symlinked ones, so a real dir
-- always registers its canonical path before a symlink to it is examined
-- -- that keeps file paths (and therefore ids) stable.
walkWithLoops :: Bool -> FilePath -> IO ([FilePath], [FilePath])
walkWithLoops follow root = do
  isDir <- doesDirectoryExist root
  if not isDir
    then pure ([], [])
    else do
      canonRoot <- canonicalizePath root
      (_visited, files, loops) <- go (Set.fromList [canonRoot]) root
      pure (files, loops)
  where
    go visited dir = do
      entries <- sort <$> listDirectory dir
      tagged <- forM entries $ \e -> do
        let p = dir </> e
        isLink <- pathIsSymbolicLink p
        pure (isLink, p)
      let ordered =
            [ t | t@(isLink, _) <- tagged, not isLink ]
            ++ [ t | t@(isLink, _) <- tagged, isLink ]
      foldM step (visited, [], []) ordered
      where
        step (vis, fs, ls) (isLink, p)
          | isLink && not follow = pure (vis, fs, ls)  -- symlink policy: skip
          | otherwise = do
              isDir <- doesDirectoryExist p             -- follows the link
              if isDir
                then do
                  canon <- canonicalizePath p
                  if isCircular vis canon
                    then pure (vis, fs, ls ++ [p])        -- loop/dup: skip + report
                    else do
                      (vis', fs', ls') <- go (Set.insert canon vis) p
                      pure (vis', fs ++ fs', ls ++ ls')
                else do
                  isFile <- doesFileExist p
                  pure (vis, fs ++ [p | isFile], ls)

-- | 'walkWithLoops' without the loop report: just the files found.
walk :: Bool -> FilePath -> IO [FilePath]
walk follow root = fst <$> walkWithLoops follow root

-- | Recursively walk @root@ and return the media FILE PATHS found (item
-- construction happens in the Monad layer via 'itemFromPath').
-- A missing directory yields [].
scanDirectory :: Bool -> FilePath -> IO [FilePath]
scanDirectory follow root = do
  files <- walk follow root
  pure (filter isMediaFile files)
