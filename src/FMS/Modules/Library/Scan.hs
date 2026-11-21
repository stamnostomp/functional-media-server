module FMS.Modules.Library.Scan
  ( scanDirectory
  , walk
  , walkWithLoops
  , isCircular
  , diffScan
  , planRescan
  , itemFromPath
  , classifyItem
  , episodeNfoSidecar
  , parseEpisodeFilename
  , parseEpisodeTagPos
  , episodeTitleFromName
  , seriesFromEpisodeName
  , extraTypeFor
  , hasTvShowNfo
  , isMediaFile
  , mimeFor
  , mediaExtensions
  , imageExtensions
  , normalizeVersionTitle
  , versionRank
  , groupVersions
  , itemFromGroup
  , chunksOf
  , statFile
  , pooledMapIO
  , isNfoFile
  , siblingMediaPathsForNfo
  , itemIdsForNfo
  ) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (foldM, forM)
import Data.Char (isAlpha, isDigit, isUpper, toLower)
import Data.Either (partitionEithers)
import Data.List (nub, sort, sortBy, sortOn)
import qualified Data.ByteString as BS
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
import System.FilePath (dropExtension, replaceExtension, takeBaseName, takeDirectory, takeExtension, takeFileName, (</>))

import FMS.Core.Types.MediaItem (MediaItem(..), itemTypeFromMime)
import FMS.Modules.Metadata.Api.Types (cleanTitleForProvider, releaseTagTokens)
import FMS.Modules.Metadata.Nfo (nfoPathFor, parseEpisodeNfo, EpisodeNfo(..))

-- | Extensions recognised as media files (lowercase, with dot).
-- Milestone 2e: photos (image/*) and books (epub/mobi/pdf/cbz/cbr) are
-- first-class library items alongside video and audio.
mediaExtensions :: [String]
mediaExtensions =
  [ ".mp4", ".mkv", ".avi", ".webm", ".mov"
  , ".mp3", ".flac", ".wav", ".ogg", ".m4a"
  , ".jpg", ".jpeg", ".png", ".gif", ".webp", ".bmp", ".heic", ".tiff"
  , ".epub", ".mobi", ".pdf", ".cbz", ".cbr"
  ]

-- | Image extensions (lowercase, with dot). m51: torrent screenshots /
-- cover art (screen0001.jpg, poster.jpg, ...) must NOT become library
-- items, so 'isMediaFile' excludes them even though 'mediaExtensions'
-- still lists them (NFO sidecar sibling lookup keeps matching poster.jpg
-- next to media, so artwork/nfo association is unaffected).
imageExtensions :: [String]
imageExtensions =
  [ ".jpg", ".jpeg", ".png", ".gif", ".webp", ".bmp", ".heic", ".tiff" ]

-- | Is this path an NFO sidecar (@.nfo@, case-insensitive)? NFO files are
-- not media, but the watcher must react to them so a changed sidecar
-- refreshes the sibling item's metadata (invalidation audit).
isNfoFile :: FilePath -> Bool
isNfoFile p = map toLower (takeExtension p) == ".nfo"

-- | Pure: the media paths that share an NFO sidecar with @nfoPath@ —
-- for @movie.nfo@ every media-extension variant of @movie@; for a
-- series-level @tvshow.nfo@ no single file (the caller refreshes all
-- episodes in the directory, see 'itemIdsForNfo').
siblingMediaPathsForNfo :: FilePath -> [FilePath]
siblingMediaPathsForNfo nfoPath
  | takeFileName nfoPath == "tvshow.nfo" = []
  | otherwise = nub [ replaceExtension (dropExtension nfoPath) ext | ext <- mediaExtensions ]

-- | Pure: the ids of library items whose metadata an NFO-sidecar change
-- at @nfoPath@ affects: items whose own sidecar this is, plus (for
-- @tvshow.nfo@) every episode item in the same directory. hinotify
-- directory watches report BARE file names (no directory), so when the
-- event path carries no directory (\".\") the match is by file name
-- alone; absolute event paths must match the directory exactly.
itemIdsForNfo :: [MediaItem] -> FilePath -> [Text]
itemIdsForNfo items nfoPath =
  nub [ itemId i | i <- items, sidecarMatches i ]
  where
    nfoName  = takeFileName nfoPath
    evDir    = takeDirectory nfoPath
    isTvShow = nfoName == "tvshow.nfo"
    dirOk itemDir = evDir `elem` [".", ""] || itemDir == evDir
    sidecarMatches i
      | isTvShow  = itemType i == "Episode" && dirOk (takeDirectory (itemPath i))
      | otherwise = takeFileName (nfoPathFor (itemPath i)) == nfoName
                    && dirOk (takeDirectory (itemPath i))

-- | Is this path a media file, judging by extension? m51: image files
-- are NOT media items (no Photo items from torrent previews); audio and
-- video stay.
isMediaFile :: FilePath -> Bool
isMediaFile p =
  let ext = map toLower (takeExtension p)
  in ext `elem` mediaExtensions && ext `notElem` imageExtensions

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
  -- milestone 2e: photos
  ".jpg"  -> "image/jpeg"
  ".jpeg" -> "image/jpeg"
  ".png"  -> "image/png"
  ".gif"  -> "image/gif"
  ".webp" -> "image/webp"
  ".bmp"  -> "image/bmp"
  ".heic" -> "image/heic"
  ".tiff" -> "image/tiff"
  -- milestone 2e: books / comics
  ".epub" -> "application/epub+zip"
  ".mobi" -> "application/x-mobipocket-ebook"
  ".pdf"  -> "application/pdf"
  ".cbz"  -> "application/vnd.comicbook+zip"
  ".cbr"  -> "application/vnd.comicbook-rar"
  _       -> "application/octet-stream"

-- | Pure (milestone 4d): detect a Kodi-style episode tag @S01E02@ /
-- @s1e2@ / @S01E02E03@ in a file base name and return the first
-- @(season, episode)@ pair. Manual scanning, no regex dependency.
parseEpisodeFilename :: Text -> Maybe (Int, Int)
parseEpisodeFilename t = parseEpisodeTagPos t >>= \(_, s, e) -> Just (s, e)

-- | m44: like 'parseEpisodeFilename' but also returns the character offset
-- where the @SxxEyy@ tag STARTS: @(start, season, episode)@.
parseEpisodeTagPos :: Text -> Maybe (Int, Int, Int)
parseEpisodeTagPos t = go 0 (T.unpack t)
  where
    go _ [] = Nothing
    go i (c : rest)
      | c == 's' || c == 'S' =
          case span isDigit rest of
            (ds, e : rest') | not (null ds), e == 'e' || e == 'E' ->
              case span isDigit rest' of
                (es, _) | not (null es) -> Just (i, read ds, read es)
                _                       -> go (i + 1) rest
            _ -> go (i + 1) rest
      | otherwise = go (i + 1) rest

-- | m73: the episode title of an episode file base name -- the part AFTER
-- the @SxxEyy@ tag with dots/underscores turned into spaces and release
-- tags / bracketed groups / the trailing release-group token dropped
-- (dashes inside words are kept: episode titles use them, e.g.
-- @Happy.Hell-o-ween.1080p...@ -> @"Happy Hell-o-ween"@). 'Nothing' when
-- nothing usable remains. Pure.
episodeTitleFromName :: Text -> Maybe Text
episodeTitleFromName baseName =
  case parseEpisodeTagPos baseName of
    Nothing -> Nothing
    Just (pos, _, _) ->
      let afterTag = dropTag (drop pos (T.unpack baseName))
          spaced   = T.map (\c -> if c == '.' || c == '_' then ' ' else c) (T.pack afterTag)
          toks0    = filter (not . T.null) (T.words spaced)
          isBracketed t = T.any (`elem` ("()[]{}" :: String)) t
          toks1    = filter (\t -> not (isBracketed t)
                                   && T.toLower t `notElem` releaseTagTokens) toks0
          -- drop a trailing release-group token ("x265-MeGusta", "-LAMA")
          toks2    = case reverse toks1 of
            (t : revRest) | T.isInfixOf (T.singleton '-') t -> reverse revRest
            _                                     -> toks1
          cleaned  = T.unwords (filter (not . T.all isDigit) toks2)
      in if T.null cleaned then Nothing else Just cleaned
  where
    dropTag (c : rest) | c == 's' || c == 'S' =
      case span isDigit rest of
        (ds, rest') | not (null ds) -> case rest' of
          (e : rest'') | e == 'e' || e == 'E' ->
            case span isDigit rest'' of
              (es, rest''') | not (null es) -> rest'''
              _                             -> rest''
          _ -> rest'
        _ -> rest
    dropTag r = r

-- | m44: the series name of an episode file base name -- the part BEFORE
-- the first @SxxEyy@ tag, cleaned of release tags via
-- 'cleanTitleForProvider'. When the tag is at the very start (empty
-- prefix) the whole base name is cleaned instead.
seriesFromEpisodeName :: Text -> Text
seriesFromEpisodeName baseName =
  let prefix = case parseEpisodeTagPos baseName of
        Just (pos, _, _) -> T.pack (take pos (T.unpack baseName))
        Nothing          -> baseName
      use = if T.null (T.strip prefix) then baseName else prefix
  in fst (cleanTitleForProvider use)

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
-- (m43) a scene-style @SxxEyy@ file name is also an 'Episode', with the
-- series name cleaned from the file name; in all other cases the type
-- comes from the mime and the episode fields are 'Nothing'.
classifyItem
  :: Bool            -- ^ hasTvShowNfo (parent dir contains tvshow.nfo)
  -> Maybe (Int, Int) -- ^ m76: (season, episode) from a per-file <base>.nfo
                     --   <episodedetails> sidecar -- wins over the file name
  -> FilePath        -- ^ the file path
  -> Text            -- ^ the file base name
  -> (Text, Maybe Text, Maybe Int, Maybe Int)
classifyItem hasNfo mEpNfo path baseName
  | Just (s, e) <- mEpNfo =
      -- m76: the request-time NFO knows the exact episode (written from the
      -- TMDB id the user picked) even when the release file name has no
      -- usable SxxEyy tag
      let series = if hasNfo
                     then T.pack (takeFileName (takeDirectory path))
                     else seriesFromEpisodeName baseName
      in ("Episode", Just series, Just s, Just e)
  | hasNfo =
      let series = T.pack (takeFileName (takeDirectory path))
          (season, episode) = case parseEpisodeFilename baseName of
            Just (s, e) -> (Just s, Just e)
            Nothing     -> (Nothing, Nothing)
      in ("Episode", Just series, season, episode)
  | Just (s, e) <- parseEpisodeFilename baseName =
      -- m43/m44: scene release without tvshow.nfo -> Episode; series name is
      -- the part of the file name BEFORE the SxxEyy tag, cleaned of tags
      let series = seriesFromEpisodeName baseName
      in ("Episode", Just series, Just s, Just e)
  | otherwise = (itemTypeFromMime (mimeFor path), Nothing, Nothing, Nothing)

-- | m76: IO: the (season, episode) from a per-file @<base>.nfo@
-- @<episodedetails>@ sidecar next to @path@, when present and parseable.
episodeNfoSidecar :: FilePath -> IO (Maybe (Int, Int))
episodeNfoSidecar p = do
  let f = replaceExtension p ".nfo"
  ok <- doesFileExist f
  if not ok
    then pure Nothing
    else do
      r <- try (BS.readFile f) :: IO (Either SomeException BS.ByteString)
      pure $ case r of
        Left _ -> Nothing
        Right b -> case parseEpisodeNfo b of
          Right en -> case (enSeason en, enEpisode en) of
            (Just s, Just e) -> Just (s, e)
            _                -> Nothing
          Left _ -> Nothing

-- | Milestone 4j quality/codec tokens stripped by 'normalizeVersionTitle'
-- (lowercase; matched as whole dot/underscore/space-separated words).
versionTokens :: [Text]
versionTokens =
  [ "1080p", "2160p", "4k", "8k", "720p", "480p"
  , "bluray", "bdrip", "brrip", "web-dl", "webdl", "webrip", "web"
  , "hdtv", "hdtvrip", "dvdrip", "remux", "uncut", "extended", "proper"
  , "repack", "imax", "x264", "x265", "h264", "hevc", "aac", "dts"
  , "ac3", "51", "20", "hdr", "sdr"
  ]

-- | Pure (milestone 4j): normalize a file base name (WITHOUT extension) for
-- multi-version grouping: bracketed groups @[1080p]@ and dots/underscores/
-- dashes become spaces (after pre-replacing the dotted forms @5.1@, @2.0@,
-- @web-dl@ so they survive as single tokens); every word in
-- 'versionTokens' (case-insensitive) is dropped, as are ALL-UPPERCASE words
-- of length >= 2 (release-group tags like @-GRP@); the rest is lowercased,
-- joined with single spaces and trimmed. E.g.
-- @\"Movie.Name.2019.1080p.BluRay.x264-GRP\"@ -> @\"movie name 2019\"@.
normalizeVersionTitle :: Text -> Text
normalizeVersionTitle base0 =
  let dotted = T.replace "web-dl" "webdl"
             (T.replace "5.1" " 51 " (T.replace "2.0" " 20 " base0))
      spaced = T.concatMap (\c -> if c `elem` (". _[]()-" :: String) then " " else T.singleton c) dotted
      keep w = let lw = T.toLower w
               in lw `notElem` versionTokens
                  && not (T.length w >= 2 && T.any isAlpha w
                           && T.all (\c -> not (isAlpha c) || isUpper c) w)
      words' = map T.toLower (filter keep (T.words spaced))
  in T.unwords words'

-- | Pure (milestone 4j): resolution rank of a file base name from its
-- tokens: 8k=5, 2160p/4k=4, 1080p=3, 720p=2, 480p=1, default 0. The maximum
-- over all tokens wins.
versionRank :: Text -> Int
versionRank base =
  let lower  = T.toLower base
      spaced = T.concatMap (\c -> if c `elem` (". _[]()-" :: String) then " " else T.singleton c) lower
      ranks  = [ r | (tok, r) <- [("8k",5),("2160p",4),("4k",4),("1080p",3),("720p",2),("480p",1)]
                   , tok `elem` T.words spaced ]
  in if null ranks then 0 else maximum ranks

-- | Pure (milestone 4j): group @(path, size)@ pairs by
-- @(directory, 'normalizeVersionTitle' baseName)@. Every member list is
-- sorted BEST FIRST: 'versionRank' desc, then size desc, then path asc.
-- The groups themselves are sorted by their primary (head) path, so the
-- output is deterministic. Callers pass only video files; non-video files
-- must not be grouped (they are handled as singletons by the caller).
groupVersions :: [(FilePath, Integer)] -> [[(FilePath, Integer)]]
groupVersions files =
  let keyed   = [ ((takeDirectory p, normalizeVersionTitle (T.pack (takeBaseName p))), [(p, s)])
                | (p, s) <- files ]
      grouped = Map.toList (Map.fromListWith (++) keyed)
      sorted  = map (sortBy versionCmp . snd) grouped
      groupPrimary g = case g of ((p, _) : _) -> p; [] -> ""  -- groups are non-empty
  in sortOn groupPrimary sorted
  where
    versionCmp (p1, s1) (p2, s2) =
      compare (versionRank (T.pack (takeBaseName p2))) (versionRank (T.pack (takeBaseName p1)))
      <> compare s2 s1
      <> compare p1 p2

-- | Build a multi-version 'MediaItem' from a group of paths sorted BEST
-- FIRST ('groupVersions' output): the head becomes the primary path, all
-- paths go into 'itemVersions'. The id is left empty (acid-state assigns
-- it). Singleton groups are built by 'itemFromPath' instead (itemVersions
-- = []).
itemFromGroup :: Text -> [FilePath] -> IO MediaItem
itemFromGroup folder paths = case paths of
  []        -> itemFromPath folder ""   -- never happens: groups are non-empty
  [p]       -> itemFromPath folder p     -- singleton: no version list
  (p : _) -> do
    item <- itemFromPath folder p
    pure item { itemVersions = paths }

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
  epNfo <- episodeNfoSidecar p
  let base  = T.pack (takeBaseName p)
      mime  = mimeFor p
      (itype, series, season, episode) = classifyItem nfo epNfo p base
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
    , itemVersions  = []   -- single-file item; multi-version groups use itemFromGroup (milestone 4j)
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

-- Parallel scan stat phase (parallel library scanning): the per-file IO
-- (size + mtime) is mapped over a bounded pool of worker threads, while
-- every later phase (acid updates, diff, item construction) stays
-- sequential in the Monad layer.

-- | Pure: split a list into chunks of at most @n@ elements, preserving
-- order. @n <= 0@ is treated as 1 (one element per chunk is NOT assumed —
-- the whole list becomes a single chunk, the safe degenerate case);
-- an empty list yields []. Exported for tests.
chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs
  | null xs   = []
  | n <= 0    = [xs]
  | otherwise = go xs
  where
    go []       = []
    go ys       = let (c, rest) = splitAt n ys in c : go rest

-- | IO: stat one file: @(path, size, mtime)@.
statFile :: FilePath -> IO (FilePath, Integer, UTCTime)
statFile p = do
  s <- getFileSize p
  t <- getModificationTime p
  pure (p, s, t)

-- | IO: map an IO action over a list with a bounded pool of @workers@
-- threads. The list is split into @workers@ chunks ('chunksOf') and the
-- chunks are processed by 'mapConcurrently' (one thread per chunk), so
-- at most @workers@ threads are alive at once and the RESULT ORDER
-- matches the input order ('mapConcurrently' preserves order, and the
-- chunks are concatenated in order). @workers <= 1@ degenerates to the
-- plain sequential 'mapM' behaviour.
pooledMapIO :: Int -> (a -> IO b) -> [a] -> IO [b]
pooledMapIO workers f xs
  | workers <= 1 = mapM f xs
  | otherwise    = concat <$> mapConcurrently (mapM f) (chunksOf workers xs)
