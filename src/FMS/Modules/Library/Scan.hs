module FMS.Modules.Library.Scan
  ( scanDirectory
  , walk
  , walkWithLoops
  , isCircular
  , diffScan
  , planRescan
  , itemFromPath
  , classifyItem
  , parseEpisodeFilename
  , extraTypeFor
  , hasTvShowNfo
  , isMediaFile
  , mimeFor
  , mediaExtensions
  ) where

import Control.Applicative ((<|>))
import Control.Monad (foldM, forM)
import Data.Char (isDigit, toLower)
import Data.Either (partitionEithers)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (listToMaybe, mapMaybe)
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
import System.FilePath (takeBaseName, takeDirectory, takeExtension, takeFileName, (</>))

import FMS.Core.Types.MediaItem (MediaItem(..), itemTypeFromMime)

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

-- | Pure (milestone 4d): detect a Kodi-style episode tag @S01E02@ /
-- @s1e2@ / @S01E02E03@ in a file base name and return the first
-- @(season, episode)@ pair. Manual scanning, no regex dependency.
parseEpisodeFilename :: Text -> Maybe (Int, Int)
parseEpisodeFilename t = go (T.unpack t)
  where
    go [] = Nothing
    go (c : rest)
      | c == 's' || c == 'S' =
          case span isDigit rest of
            (ds, e : rest') | not (null ds), e == 'e' || e == 'E' ->
              case span isDigit rest' of
                (es, _) | not (null es) -> Just (read ds, read es)
                _                       -> go rest
            _ -> go rest
      | otherwise = go rest

-- | IO: does the file's parent directory contain a @tvshow.nfo@?
hasTvShowNfo :: FilePath -> IO Bool
hasTvShowNfo path = doesFileExist (takeDirectory path </> "tvshow.nfo")

-- | Pure (milestone 4g): classify a file as an Extra (trailer,
-- behind-the-scenes, ...) or not. The second argument is the file's base
-- name WITHOUT extension (matched case-insensitively); the first is the
-- full path, whose parent directory names are also scanned. Token rules,
-- checked in order, first match wins: the file base name is checked
-- against every token first, then the parent directory names; a parent
-- directory named exactly @extras@ or @specials@ yields @Just \"Other\"@.
extraTypeFor :: FilePath -> Text -> Maybe Text
extraTypeFor path baseName =
  listToMaybe (mapMaybe (`matchToken` lowerBase) tokens)
    <|> listToMaybe [ res | (needles, res) <- tokens, d <- dirNames
                         , any (`T.isInfixOf` d) needles ]
    <|> (if any (`elem` ["extras", "specials"]) dirNames then Just "Other" else Nothing)
  where
    tokens =
      [ (["trailer"], "Trailer")
      , (["behind the scenes", "behindthescenes", "bts"], "BehindTheScenes")
      , (["deleted scene", "deletedscene"], "DeletedScene")
      , (["featurette"], "Featurette")
      , (["short"], "Short")
      , (["theme"], "ThemeSong")
      ]
    matchToken (needles, res) txt = if any (`T.isInfixOf` txt) needles then Just res else Nothing
    lowerBase = T.toLower baseName
    dirNames = map (T.toLower . T.pack) (parentDirNames path)

-- | Pure: the names of all parent directories of a path, innermost
-- first (@/a/b/c.mp4@ -> [@\"b\",\"a\"]@).
parentDirNames :: FilePath -> [FilePath]
parentDirNames p = go (takeDirectory p)
  where
    go d
      | null d || d == "/" || d == "." = []
      | otherwise = takeFileName d : go (takeDirectory d)

-- | Pure (milestone 4d): classify a scanned file into
-- @(itemType, series, season, episode)@. When the parent directory has a
-- @tvshow.nfo@ the file is an 'Episode' of the series named after the
-- directory, with season/episode from 'parseEpisodeFilename'; otherwise
-- the type comes from the mime and the episode fields are 'Nothing'.
classifyItem
  :: Bool      -- ^ hasTvShowNfo (parent dir contains tvshow.nfo)
  -> FilePath  -- ^ the file path
  -> Text      -- ^ the file base name
  -> (Text, Maybe Text, Maybe Int, Maybe Int)
classifyItem hasNfo path baseName
  | hasNfo =
      let series = T.pack (takeFileName (takeDirectory path))
          (season, episode) = case parseEpisodeFilename baseName of
            Just (s, e) -> (Just s, Just e)
            Nothing     -> (Nothing, Nothing)
      in ("Episode", Just series, season, episode)
  | otherwise = (itemTypeFromMime (mimeFor path), Nothing, Nothing, Nothing)

-- | Build a 'MediaItem' from a path: size + mtime from the filesystem,
-- title = base name, mime from the extension, classification via
-- 'classifyItem' (milestone 4d: tvshow.nfo -> Episode with series/season/
-- episode, else mime-based type), tagged with the id of the media folder
-- it was scanned from (milestone 2c). Milestone 4g: for video files an
-- 'extraTypeFor' match WINS over the Episode classification -- a trailer
-- inside a show directory is an Extra, not an Episode -- and sets
-- itemType = \"Extra\" with itemExtraType = the match. The id is left
-- empty ("") -- the acid-state 'RegisterItem' event assigns the stable id.
itemFromPath :: Text -> FilePath -> IO MediaItem
itemFromPath folder p = do
  size  <- getFileSize p
  mtime <- getModificationTime p
  nfo   <- hasTvShowNfo p
  let base  = T.pack (takeBaseName p)
      mime  = mimeFor p
      (itype, series, season, episode) = classifyItem nfo p base
      (finalType, extra, finalSeries, finalSeason, finalEpisode) =
        case extraTypeFor p base of
          Just et | "video" `T.isPrefixOf` mime -> ("Extra", Just et, Nothing, Nothing, Nothing)
          _                                      -> (itype, Nothing, series, season, episode)
  pure MediaItem
    { itemId        = ""
    , itemTitle     = base
    , itemPath      = p
    , itemSize      = size
    , itemMime      = mime
    , itemAddedAt   = mtime
    , itemFolder    = folder
    , itemType      = finalType
    , itemSeries    = finalSeries
    , itemSeason    = finalSeason
    , itemEpisode   = finalEpisode
    , itemExtraType = extra
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
