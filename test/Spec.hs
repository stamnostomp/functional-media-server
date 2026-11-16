module Main (main) where

import Control.Monad (when)
import Data.Acid (closeAcidState, openLocalStateFrom, query, update)
import Data.Acid.Local (createCheckpointAndClose)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.HashMap.Strict as HM
import Data.List (isInfixOf, isPrefixOf, sort)
import Data.Maybe (fromMaybe, listToMaybe)
import qualified Data.Set as Set
import Data.SafeCopy (safeGet, safePut)
import Data.Serialize (runGet, runPut)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , findExecutable
  , removeDirectoryRecursive
  , removeFile
  , setModificationTime
  )
import System.Posix.Files (createSymbolicLink)
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath ((</>))

import FMS.Core.Module (Segment (..), get, post)
import FMS.Core.Process (runProcessBS, showBS)
import FMS.Core.Query (applyItemQuery, applyMetadataFilters, applyUserDataFilters, attachUserData, filterByFolder, filterByIncludeTypes, filterBySeries, filterByType, filterByYears, itemYear, paginate, projectFields, queryParams, searchItems, showsFromItems, sortEpisodes, sortItems, specialFeaturesOf)
import FMS.Core.Range (ByteRange(..), parseRangeHeader)
import FMS.Core.RateLimit (Bucket (..), newBucket, refill, requestIdHex, tryTake)
import FMS.Core.Router (isOpenPath, matchSegments, parseTokenHeader)
import FMS.Core.Types.MediaFolder (MediaFolder (..))
import FMS.Core.Types.MediaItem (MediaItem (..), MediaItemV0 (..), MediaItemV1 (..), MediaItemV2 (..), itemTypeFromMime, migrateMediaItem, migrateMediaItemV1, migrateMediaItemV2)
import FMS.Core.Types.Progress (ItemStats(..), Progress(..))
import FMS.Core.Types.ServerConfig
  ( ServerConfig (..)
  , ServerConfigPatch (..)
  , applyServerConfigPatch
  , defaultServerConfig
  , loadServerConfig
  , mergeServerConfig
  , saveServerConfig
  )
import FMS.Core.Types.User (User(..), toInternalJSON)
import FMS.Modules.Auth.Monad (canAccessFolder, hashPassword, verifyPassword)
import FMS.Modules.Auth.Routes (PermissionsRequest (..))
import FMS.Modules.Auth.State (loadUsers, saveUsers)
import FMS.Modules.Library.Monad (withScanLock)
import FMS.Modules.Library.Folders (loadFolders, nextFolderId, saveFolders)
import FMS.Modules.Library.Scan
  ( classifyItem
  , diffScan
  , extraTypeFor
  , isCircular
  , isMediaFile
  , parseEpisodeFilename
  , itemFromPath
  , mimeFor
  , planRescan
  , scanDirectory
  , walk
  , walkWithLoops
  )
import FMS.Modules.Stats.Monad (applyPlay, applyProgress, continueWatching, popular)
import FMS.Modules.Stats.Persistence (loadStats, saveStats)
import FMS.Modules.UserData.Monad (markPlayed, setFavorite, setPosition)
import FMS.Modules.UserData.Persistence (loadUserData, saveUserData)
import FMS.Modules.UserData.Types (UserData (..), emptyUserData)
import FMS.Modules.Hls (hlsArgs, hlsDirFor)
import FMS.Modules.Images (imageExtFor, imagePathFor, isValidImageType, resizeArgs)
import FMS.Modules.Stream (resolveRange)
import FMS.Modules.Subtitles (SubtitleSource (..), embeddedSubtitleSources, embeddedToVttArgs, indexSources, pickSubtitles, subtitleExts, subtitleList, subtitleListSources, subtitleToVttArgs)
import FMS.Modules.Thumbs (thumbArgs, thumbPathFor)
import FMS.Modules.Transcode (transcodeArgs)
import FMS.Modules.Library.State
  ( GetAllItems(..)
  , GetIdByPath(..)
  , GetItemById(..)
  , ListPaths(..)
  , RegisterItem(..)
  , RemovePaths(..)
  , UpdateItemMeta(..)
  , initialLibraryState
  )
import FMS.Modules.Metadata.Api.Tmdb (parseTmdbResults, tmdbSearchUrl)
import FMS.Modules.Metadata.Api.Types (TmdbResult (..))
import FMS.Modules.Metadata.Nfo (nfoPathFor, parseMovieNfo, parseTvShowNfo)
import FMS.Modules.Metadata.Persistence (loadMetadata, saveMetadata)
import FMS.Modules.Metadata.Routes (genresFromMetadata, personsFromMetadata, studiosFromMetadata, tagsFromMetadata)
import FMS.Modules.Metadata.Types (MediaMetadata (..))
import FMS.Modules.Playlists.Monad (addItem, removeItem)
import FMS.Modules.Playlists.Persistence (loadPlaylists, savePlaylists)
import FMS.Modules.Playlists.Types (Playlist (..))
import FMS.Modules.Playback.Probe
  ( containerFromExtension
  , directPlayable
  , directStreamable
  , parseFfprobe
  , probeArgs
  , probeFile
  , transcoding
  )
import FMS.Modules.Playback.Types (MediaStream(..))
import FMS.Modules.Sessions.Monad (expireSessions, upsertSession)
import FMS.Modules.Sessions.Types (PlaySession (..))
import FMS.Modules.Admin.Monad (activityLogCap, pushActivity, taskDue)
import FMS.Modules.Admin.Types (ActivityEntry (..), ScheduledTask (..))
import FMS.Modules.ApiDocs (methodText, routeToPath, routesToOpenApi, segmentToPathPiece)
import FMS.Modules.Web (webPageHtml)

-- | Plain base-only assertions (no extra test deps): print PASS/FAIL per
-- case and exit non-zero on the first failure.
check :: (Eq a, Show a) => String -> a -> a -> IO ()
check label expected actual
  | expected == actual = putStrLn ("PASS: " ++ label)
  | otherwise = do
      putStrLn ("FAIL: " ++ label
        ++ " -- expected " ++ show expected ++ ", got " ++ show actual)
      exitFailure

-- | Remove a scratch dir under /tmp if it exists.
resetDir :: FilePath -> IO ()
resetDir dir = do
  exists <- doesDirectoryExist dir
  if exists then removeDirectoryRecursive dir else pure ()

main :: IO ()
main = do
  -- Range header parsing (docs/modules.org: malformed means "no range").
  check "bytes=0-100"  (Just (ByteRange 0 (Just 100))) (parseRangeHeader "bytes=0-100")
  check "bytes=200-"   (Just (ByteRange 200 Nothing)) (parseRangeHeader "bytes=200-")
  check "bytes=abc"    Nothing                       (parseRangeHeader "bytes=abc")
  check "empty header" Nothing                       (parseRangeHeader "")
  -- "bytes=5-2" has end < start: invalid, treated as no range.
  check "bytes=5-2 (end < start -> Nothing)" Nothing (parseRangeHeader "bytes=5-2")
  check "no bytes= prefix"                    Nothing (parseRangeHeader "0-100")
  check "suffix range bytes=-500"             Nothing (parseRangeHeader "bytes=-500")

  -- resolveRange against a 1000-byte file (stream module, milestone 3):
  -- valid ranges clamp to (start, end) inclusive, invalid ones are Left.
  let isLeftE :: Either a b -> Bool
      isLeftE (Left _)  = True
      isLeftE (Right _) = False
  check "resolveRange (0, Just 99)"   (Right (0, 99))   (resolveRange 1000 (ByteRange 0 (Just 99)))
  check "resolveRange (900, Nothing)" (Right (900, 999)) (resolveRange 1000 (ByteRange 900 Nothing))
  check "resolveRange (0, Nothing)"   (Right (0, 999))  (resolveRange 1000 (ByteRange 0 Nothing))
  check "resolveRange (5000, Just 6000) is Left (start >= size)"
    True (isLeftE (resolveRange 1000 (ByteRange 5000 (Just 6000))))
  check "resolveRange (5, Just 2) is Left (end < start)"
    True (isLeftE (resolveRange 1000 (ByteRange 5 (Just 2))))
  check "resolveRange (1000, Nothing) is Left (start >= size)"
    True (isLeftE (resolveRange 1000 (ByteRange 1000 Nothing)))
  check "resolveRange (0, Just 1000) is Left (end >= size)"
    True (isLeftE (resolveRange 1000 (ByteRange 0 (Just 1000))))

  -- Route segment matching.
  check "literal match"        (Just [])                  (matchSegments [Lit "a", Lit "b"] ["a", "b"])
  check "capture binds"        (Just [("id", "42")])      (matchSegments [Lit "stream", Capture "id"] ["stream", "42"])
  check "literal mismatch"     Nothing                    (matchSegments [Lit "a"] ["b"])
  check "too few segments"     Nothing                    (matchSegments [Lit "a", Lit "b"] ["a"])
  check "too many segments"    Nothing                    (matchSegments [Lit "a"] ["a", "b"])
  check "empty pattern"        (Just [])                  (matchSegments [] [])

  -- Filesystem scanner (docs/roadmap.org milestone 2): recursive walk,
  -- media-extension filter, mime map. notes.txt must be ignored.
  let scanDir = "/tmp/fms-test-scan"
  resetDir scanDir
  createDirectoryIfMissing True (scanDir </> "Movies")
  writeFile (scanDir </> "Movies" </> "a.mp4") "dummy video"  -- 11 bytes
  writeFile (scanDir </> "song.mp3") "dummy audio"           -- 11 bytes
  writeFile (scanDir </> "notes.txt") "not media"
  scanned <- scanDirectory False scanDir
  check "scan finds only media file paths"
    (sort [scanDir </> "Movies" </> "a.mp4", scanDir </> "song.mp3"])
    (sort scanned)
  scannedItems <- mapM (itemFromPath "1") scanned
  check "scan titles are base names" ["a", "song"] (sort (map (T.unpack . itemTitle) scannedItems))
  check "scan sizes" [11, 11] (sort (map itemSize scannedItems))
  check "scan mimes" ["audio/mpeg", "video/mp4"] (sort (map (T.unpack . itemMime) scannedItems))
  check "scan assigns empty id (RegisterItem fills it)" ["", ""] (sort (map (T.unpack . itemId) scannedItems))
  check "mimeFor .mp4" "video/mp4" (mimeFor (scanDir </> "Movies" </> "a.mp4"))
  check "mimeFor .mp3" "audio/mpeg" (mimeFor (scanDir </> "song.mp3"))
  check "isMediaFile .txt is False" False (isMediaFile (scanDir </> "notes.txt"))
  check "isMediaFile .MKV (case-insensitive) is True" True (isMediaFile "/x/Y.MKV")
  missing <- scanDirectory False "/tmp/fms-test-scan-does-not-exist"
  check "scan of missing dir is []" [] missing
  removeDirectoryRecursive scanDir

  -- Incremental rescan pure helpers (milestone 2b, docs/todo.org section 2).
  check "diffScan unchanged" ([], []) (diffScan ["a", "b"] ["b", "a"])
  check "diffScan add/remove" (["a"], ["c"]) (diffScan ["a", "b"] ["b", "c"])
  check "diffScan empty" ([], []) (diffScan [] [])
  check "diffScan all new" ([], ["x", "y"]) (diffScan [] ["y", "x"])
  check "isCircular hit" True (isCircular (Set.fromList ["/x"]) "/x")
  check "isCircular miss" False (isCircular (Set.fromList ["/x"]) "/y")
  check "isCircular empty" False (isCircular Set.empty "/x")
  tNow <- getCurrentTime
  let pt1 = tNow
      pt2 = addUTCTime 5 tNow
  check "planRescan mixed (removed, changedOrNew, unchanged)"
    (["gone"], ["changed", "new"], ["same"])
    (planRescan
      [("same", 10, pt1), ("changed", 20, pt2), ("new", 5, pt1)]
      [("same", Just (10, pt1)), ("changed", Just (10, pt1)), ("gone", Just (1, pt1))])
  check "planRescan no existing (all new)"
    ([], ["a", "b"], [])
    (planRescan [("a", 1, pt1), ("b", 2, pt1)] [])
  check "planRescan all removed"
    (["x"], [], [])
    (planRescan [] [("x", Just (1, pt1))])
  check "planRescan unknown existing meta counts as changed"
    ([], ["mystery"], [])
    (planRescan [("mystery", 3, pt1)] [("mystery", Nothing)])

  -- Symlink walk policy (milestone 2b): follow=False skips all symlinks;
  -- follow=True follows file symlinks and detects circular dir chains.
  let linkDir = "/tmp/fms-test-symlinks"
  resetDir linkDir
  createDirectoryIfMissing True (linkDir </> "real")
  writeFile (linkDir </> "real" </> "a.mp4") "x"
  createSymbolicLink (linkDir </> "real" </> "a.mp4") (linkDir </> "link.mp4")
  createSymbolicLink (linkDir </> "real") (linkDir </> "real" </> "self")
  createSymbolicLink (linkDir </> "real") (linkDir </> "dup")
  noFollow <- walk False linkDir
  check "walk False skips symlinks entirely"
    [linkDir </> "real" </> "a.mp4"] (sort noFollow)
  scanNoFollow <- scanDirectory False linkDir
  check "scanDirectory False skips symlinks"
    [linkDir </> "real" </> "a.mp4"] (sort scanNoFollow)
  (followFiles, followLoops) <- walkWithLoops True linkDir
  check "walk True follows file symlinks (link path reported)"
    (sort [linkDir </> "real" </> "a.mp4", linkDir </> "link.mp4"]) (sort followFiles)
  check "walk True detects circular chain AND dir-symlink duplicate of a visited dir"
    (sort [linkDir </> "real" </> "self", linkDir </> "dup"]) (sort followLoops)
  check "walk of a missing dir is []" [] =<< walk True "/tmp/fms-test-symlinks-gone"
  removeDirectoryRecursive linkDir

  -- Scan lock (milestone 2b): creates/removes the lock dir around the
  -- action; recovers a stale (> 60s) lock.
  let lockDir = "/tmp/fms-test-scan.lock"
  resetDir lockDir
  heldInside <- withScanLock lockDir (doesDirectoryExist lockDir)
  check "withScanLock holds the lock dir inside the action" True heldInside
  heldAfter <- doesDirectoryExist lockDir
  check "withScanLock releases the lock dir after the action" False heldAfter
  createDirectoryIfMissing True lockDir
  staleTime <- getCurrentTime
  setModificationTime lockDir (addUTCTime (-120) staleTime)
  recovered <- withScanLock lockDir (pure True)
  check "withScanLock recovers a stale lock" True recovered
  releasedAfter <- doesDirectoryExist lockDir
  check "withScanLock cleans up after stale recovery" False releasedAfter
  resetDir lockDir

  -- acid-state persistence + stable ids (docs/acid-state.org test).
  let acidDir = "/tmp/fms-test-acid"
  resetDir acidDir
  createDirectoryIfMissing True acidDir
  now <- getCurrentTime
  let testItem = MediaItem
        { itemId      = ""  -- assigned by RegisterItem
        , itemTitle   = "x"
        , itemPath    = "/x.mp4"
        , itemSize    = 42
        , itemMime    = "video/mp4"
        , itemAddedAt = now
        , itemFolder  = "1"
        , itemType      = "Movie"
        , itemSeries    = Nothing
        , itemSeason    = Nothing
        , itemEpisode   = Nothing
        , itemExtraType = Nothing
        }
  st1 <- openLocalStateFrom acidDir initialLibraryState
  id1 <- update st1 (RegisterItem "/x.mp4" testItem)
  id1again <- update st1 (RegisterItem "/x.mp4" testItem)
  check "registerItem is idempotent (same run)" id1 id1again
  createCheckpointAndClose st1
  -- Reopen from disk: the id mapping must have survived the restart.
  st2 <- openLocalStateFrom acidDir initialLibraryState
  mId2 <- query st2 (GetIdByPath "/x.mp4")
  check "id survives checkpoint+reopen" (Just id1) mId2
  id3 <- update st2 (RegisterItem "/x.mp4" testItem)
  check "registerItem is idempotent (after reopen)" id1 id3
  mItem <- query st2 (GetItemById id1)
  check "GetItemById returns item with assigned id"
    (Just (testItem { itemId = id1 })) mItem
  allItems <- query st2 GetAllItems
  check "GetAllItems has exactly one item" 1 (length allItems)
  -- Milestone 2b acid events: incremental rescan touches.
  paths <- query st2 ListPaths
  check "ListPaths returns the registered path" ["/x.mp4"] paths
  let later = addUTCTime 10 now
  changed <- update st2 (UpdateItemMeta "/x.mp4" 43 later)
  check "UpdateItemMeta reports a change" True changed
  unchanged <- update st2 (UpdateItemMeta "/x.mp4" 43 later)
  check "UpdateItemMeta reports no-op on same size/mtime" False unchanged
  mItem2 <- query st2 (GetItemById id1)
  check "UpdateItemMeta stored the new size/mtime"
    (Just (testItem { itemId = id1, itemSize = 43, itemAddedAt = later })) mItem2
  noPath <- update st2 (UpdateItemMeta "/nope.mp4" 1 now)
  check "UpdateItemMeta on unknown path is False" False noPath
  removed <- update st2 (RemovePaths ["/x.mp4", "/nope.mp4"])
  check "RemovePaths counts only known paths" 1 removed
  pathsAfter <- query st2 ListPaths
  check "RemovePaths removed the path" [] pathsAfter
  mGone <- query st2 (GetItemById id1)
  check "RemovePaths removed the item" Nothing mGone
  id4 <- update st2 (RegisterItem "/x.mp4" testItem)
  check "re-register after removal allocates a fresh id" True (id4 /= id1)
  closeAcidState st2
  removeDirectoryRecursive acidDir

  -- FMS.Core.Process (milestone 6a encoding bugfix): subprocess pipes are
  -- read as raw bytes and rendered with showBS, which maps one byte to
  -- one char (per-byte latin1) and never throws -- not even for multi-byte
  -- UTF-8 sequences such as the em-dash (0xE2 0x80 0x94).
  check "showBS of UTF-8 em-dash bytes [0xE2,0x80,0x94] yields a 3-char String (no throw)"
    3 (length (showBS (BS.pack [0xE2, 0x80, 0x94])))
  check "showBS maps bytes 0xE2 0x80 0x94 to chars U+00E2 U+0080 U+0094"
    "\xE2\x80\x94" (showBS (BS.pack [0xE2, 0x80, 0x94]))

  -- Transcode module (milestone 3b): the pure ffmpeg argument list.
  let targs = transcodeArgs "/tmp/fms-test-tc.wav"
  check "transcodeArgs contains -f mp4" True ("-f" `elem` targs && "mp4" `elem` targs)
  check "transcodeArgs contains pipe:1" True ("pipe:1" `elem` targs)
  check "transcodeArgs contains the input path" True ("/tmp/fms-test-tc.wav" `elem` targs)

  -- IO test of the actual transcode, guarded by ffmpeg availability: skip
  -- gracefully when ffmpeg is not on PATH.
  mFFmpeg <- findExecutable "ffmpeg"
  case mFFmpeg of
    Nothing -> putStrLn "SKIP: ffmpeg not found on PATH -- skipping transcode IO test"
    Just _ -> do
      let wav = "/tmp/fms-test-tc.wav"
      (ecGen, _, genErr) <- runProcessBS "ffmpeg"
        ["-f", "lavfi", "-i", "sine=frequency=440:duration=1", "-y", wav]
      check "generate 1s test wav: exit Success" ExitSuccess ecGen
      when (ecGen /= ExitSuccess)
        (putStrLn ("ffmpeg wav gen stderr: " ++ show (showBS (LBS.toStrict genErr))))
      (ec, out, tcErr) <- runProcessBS "ffmpeg" (transcodeArgs wav)
      check "transcodeArgs run on wav: exit Success" ExitSuccess ec
      when (ec /= ExitSuccess)
        (putStrLn ("ffmpeg transcode stderr: " ++ show (showBS (LBS.toStrict tcErr))))
      check "transcoded output is > 1000 bytes" True (LBS.length out > 1000)
      -- An MP4 starts with a size box whose type at offset 4 is "ftyp"
      -- (0x66 0x74 0x79 0x70 = [102,116,121,112]).
      check "output bytes 4..8 are the ftyp box type"
        [0x66, 0x74, 0x79, 0x70] (map fromEnum (LBS.unpack (LBS.take 4 (LBS.drop 4 out))))

  -- HLS module (milestone 3c): the pure ffmpeg argument list + dir helper.
  let hlsIn  = "/tmp/fms-test-hls-src.mp4"
      hlsOut = "/tmp/fms-test-hls-out/main.m3u8"
      hargs  = hlsArgs hlsIn hlsOut
      containsPair x y = any (\(a, b) -> a == x && b == y) (zip hargs (drop 1 hargs))
  check "hlsArgs has -hls_playlist_type vod" True (containsPair "-hls_playlist_type" "vod")
  check "hlsArgs has -c copy"               True (containsPair "-c" "copy")
  check "hlsArgs has -hls_time 4"           True (containsPair "-hls_time" "4")
  check "hlsArgs ends with the output m3u8 path" hlsOut (last hargs)
  check "hlsArgs contains the input path"   True (hlsIn `elem` hargs)
  check "hlsArgs has the seg%03d.ts pattern in the output dir"
    True ("/tmp/fms-test-hls-out/seg%03d.ts" `elem` hargs)
  check "hlsDirFor joins stateDir/hls/id" "/state/hls/42" (hlsDirFor "/state" "42")

  -- IO test of the actual HLS packaging, guarded by ffmpeg availability:
  -- skip gracefully when ffmpeg is not on PATH.
  case mFFmpeg of
    Nothing -> putStrLn "SKIP: ffmpeg not found on PATH -- skipping HLS IO test"
    Just _ -> do
      let mp4    = "/tmp/fms-test-hls-src.mp4"
          outDir = "/tmp/fms-test-hls-out"
          m3u8   = outDir </> "main.m3u8"
      resetDir outDir
      createDirectoryIfMissing True outDir
      (ecGen, _, genErr) <- runProcessBS "ffmpeg"
        [ "-f", "lavfi", "-i", "testsrc=duration=4:size=160x120:rate=10"
        , "-c:v", "libx264", "-y", mp4 ]
      check "generate 4s test mp4: exit Success" ExitSuccess ecGen
      when (ecGen /= ExitSuccess)
        (putStrLn ("ffmpeg mp4 gen stderr: " ++ show (showBS (LBS.toStrict genErr))))
      (ec, _, hlsErr) <- runProcessBS "ffmpeg" (hlsArgs mp4 m3u8)
      check "hlsArgs run on 4s mp4: exit Success" ExitSuccess ec
      when (ec /= ExitSuccess)
        (putStrLn ("ffmpeg hls stderr: " ++ show (showBS (LBS.toStrict hlsErr))))
      m3u8Exists <- doesFileExist m3u8
      check "main.m3u8 exists after ffmpeg" True m3u8Exists
      playlist <- LBS.readFile m3u8
      check "playlist starts with #EXTM3U" True (LBSC.isPrefixOf "#EXTM3U" playlist)
      let segNames = filter (LBSC.isSuffixOf ".ts") (LBSC.lines playlist)
      check "playlist references at least one .ts segment" True (not (null segNames))
      segsExist <- mapM (\s -> doesFileExist (outDir </> LBSC.unpack s)) segNames
      check "all referenced .ts segments exist on disk" True (and segsExist)
      removeDirectoryRecursive outDir
      removeFile mp4

  -- Subtitles module (milestone 3d): pure pickSubtitles / subtitleToVttArgs.
  check "subtitleExts" [".srt", ".vtt", ".ass", ".ssa", ".sub"] subtitleExts
  let dirEnts = ["clip.srt", "clip.vtt", "other.ass", "notes.txt", "clip.mp4"]
  check "pickSubtitles prefers basename matches, sorted"
    ["/m/clip.srt", "/m/clip.vtt"]
    (pickSubtitles "/m/clip.mp4" dirEnts)
  check "pickSubtitles: no basename match -> all subtitle files sorted"
    ["/m/a.ass", "/m/b.srt"]
    (pickSubtitles "/m/clip.mp4" ["b.srt", "a.ass", "notes.txt", "clip.mp4"])
  check "pickSubtitles: none -> []"
    []
    (pickSubtitles "/m/clip.mp4" ["notes.txt", "clip.mp4"])
  check "pickSubtitles is case-insensitive on extensions"
    ["/m/clip.SRT"]
    (pickSubtitles "/m/clip.mp4" ["clip.SRT", "clip.MP4"])
  let vttArgs = subtitleToVttArgs "/tmp/fms-sub-test.srt"
      vttHasPair x y = any (\(a, b) -> a == x && b == y) (zip vttArgs (drop 1 vttArgs))
  check "subtitleToVttArgs contains -f webvtt" True (vttHasPair "-f" "webvtt")
  check "subtitleToVttArgs contains pipe:1"   True ("pipe:1" `elem` vttArgs)
  check "subtitleToVttArgs contains the input path"
    True ("/tmp/fms-sub-test.srt" `elem` vttArgs)

  -- IO test of the actual SRT -> WebVTT conversion, guarded by ffmpeg
  -- availability: skip gracefully when ffmpeg is not on PATH.
  case mFFmpeg of
    Nothing -> putStrLn "SKIP: ffmpeg not found on PATH -- skipping subtitle IO test"
    Just _ -> do
      let srt = "/tmp/fms-sub-test.srt"
      writeFile srt "1\n00:00:01,000 --> 00:00:02,000\nHello\n"
      (ec, out, subErr) <- runProcessBS "ffmpeg" (subtitleToVttArgs srt)
      check "subtitleToVttArgs run on srt: exit Success" ExitSuccess ec
      when (ec /= ExitSuccess)
        (putStrLn ("ffmpeg subtitle stderr: " ++ show (showBS (LBS.toStrict subErr))))
      check "converted output contains WEBVTT" True (BS.isInfixOf "WEBVTT" (LBS.toStrict out))
      removeFile srt

  -- Thumbs module (milestone 3e): the pure ffmpeg argument list + cache
  -- path helper.
  let thumbIn = "/tmp/fms-test-thumb-src.mp4"
      thArgs  = thumbArgs thumbIn
      thumbHasPair x y = any (\(a, b) -> a == x && b == y) (zip thArgs (drop 1 thArgs))
  check "thumbArgs has -ss 5"            True (thumbHasPair "-ss" "5")
  check "thumbArgs has -i <path>"        True (thumbHasPair "-i" thumbIn)
  check "thumbArgs has -frames:v 1"      True (thumbHasPair "-frames:v" "1")
  check "thumbArgs has -f image2"        True (thumbHasPair "-f" "image2")
  check "thumbArgs has -vcodec mjpeg"    True (thumbHasPair "-vcodec" "mjpeg")
  check "thumbArgs contains pipe:1"      True ("pipe:1" `elem` thArgs)
  check "thumbPathFor joins stateDir/thumbs/id.jpg"
    "/state/thumbs/7.jpg" (thumbPathFor "/state" "7")

  -- IO test of the actual thumbnail extraction, guarded by ffmpeg
  -- availability: skip gracefully when ffmpeg is not on PATH.
  -- NOTE: thumbArgs seeks to 5s, so the source must be longer than 5s —
  -- on a 2s clip ffmpeg 9.0.2 exits 234 with empty output (verified
  -- empirically), hence the 6s lavfi source here.
  case mFFmpeg of
    Nothing -> putStrLn "SKIP: ffmpeg not found on PATH -- skipping thumbs IO test"
    Just _ -> do
      let mp4 = "/tmp/fms-test-thumb-src.mp4"
      (ecGen, _, genErr) <- runProcessBS "ffmpeg"
        [ "-f", "lavfi", "-i", "testsrc=duration=6:size=160x120:rate=10"
        , "-c:v", "libx264", "-y", mp4 ]
      check "generate 6s test mp4: exit Success" ExitSuccess ecGen
      when (ecGen /= ExitSuccess)
        (putStrLn ("ffmpeg mp4 gen stderr: " ++ show (showBS (LBS.toStrict genErr))))
      (ec, out, thErr) <- runProcessBS "ffmpeg" (thumbArgs mp4)
      check "thumbArgs run on 6s mp4: exit Success" ExitSuccess ec
      when (ec /= ExitSuccess)
        (putStrLn ("ffmpeg thumb stderr: " ++ show (showBS (LBS.toStrict thErr))))
      -- A JPEG starts with the SOI marker 0xFF 0xD8.
      check "thumb output starts with JPEG SOI (FFD8)"
        [0xFF, 0xD8] (map fromEnum (LBS.unpack (LBS.take 2 out)))
      check "thumb output is > 1000 bytes" True (LBS.length out > 1000)
      removeFile mp4

  -- Images module (milestone 6b, docs/todo.org section 5): the pure
  -- helpers -- extension mapping, type validation, ffmpeg resize args,
  -- path joining.
  check "imageExtFor image/png -> .png"        ".png" (imageExtFor "image/png")
  check "imageExtFor image/jpeg -> .jpg"       ".jpg" (imageExtFor "image/jpeg")
  check "imageExtFor image/jpg -> .jpg"        ".jpg" (imageExtFor "image/jpg")
  check "imageExtFor image/webp -> .webp"      ".webp" (imageExtFor "image/webp")
  check "imageExtFor unknown type -> .jpg"     ".jpg" (imageExtFor "application/octet-stream")
  check "imageExtFor empty header -> .jpg"     ".jpg" (imageExtFor "")
  check "imageExtFor strips parameters (image/png; charset=binary)"
    ".png" (imageExtFor "image/png; charset=binary")
  check "imageExtFor is case-insensitive (IMAGE/PNG)" ".png" (imageExtFor "IMAGE/PNG")
  check "isValidImageType Primary is True"   True  (isValidImageType "Primary")
  check "isValidImageType Backdrop is True"  True  (isValidImageType "Backdrop")
  check "isValidImageType empty is False"    False (isValidImageType "")
  check "isValidImageType 1nvalid! is False" False (isValidImageType "1nvalid!")
  check "isValidImageType with dash is False" False (isValidImageType "Primary-50")
  check "isValidImageType with slash is False" False (isValidImageType "a/b")
  check "isValidImageType with dotdot is False" False (isValidImageType "..")
  let rargs  = resizeArgs "/in.png" (Just 50) Nothing 90 "/out.jpg"
      rargs2 = resizeArgs "/in.png" (Just 50) (Just 40) 80 "/out2.jpg"
      rargs3 = resizeArgs "/in.png" Nothing (Just 40) 80 "/out3.jpg"
      hasPair x y = any (\(a, b) -> a == x && b == y) (zip rargs (drop 1 rargs))
      hasPair2 x y = any (\(a, b) -> a == x && b == y) (zip rargs2 (drop 1 rargs2))
      hasPair3 x y = any (\(a, b) -> a == x && b == y) (zip rargs3 (drop 1 rargs3))
  check "resizeArgs has -i <input>" True (hasPair "-i" "/in.png")
  check "resizeArgs ends with the output path" "/out.jpg" (last rargs)
  check "resizeArgs width-only uses scale=50:-2" True (hasPair "-vf" "scale=50:-2")
  check "resizeArgs has -q:v 90" True (hasPair "-q:v" "90")
  check "resizeArgs both dims uses force_original_aspect_ratio=decrease"
    True (hasPair2 "-vf" "scale=50:40:force_original_aspect_ratio=decrease")
  check "resizeArgs height-only uses scale=-2:40" True (hasPair3 "-vf" "scale=-2:40")
  check "imagePathFor joins stateDir/images/id/type.ext"
    "/state/images/7/Primary.png" (imagePathFor "/state" "7" "Primary" ".png")

  -- IO test of the actual resize, guarded by ffmpeg/ffprobe availability:
  -- generate a 200x100 PNG, resize to 50 wide, assert success + output
  -- exists + ffprobe reports width 50.
  case mFFmpeg of
    Nothing -> putStrLn "SKIP: ffmpeg not found on PATH -- skipping images IO test"
    Just _ -> do
      let png = "/tmp/fms-test-img-src.png"
          out = "/tmp/fms-test-img-out.jpg"
      (ecGen, _, genErr2) <- runProcessBS "ffmpeg"
        ["-f", "lavfi", "-i", "color=red:s=200x100", "-frames:v", "1", "-y", png]
      check "generate 200x100 test png: exit Success" ExitSuccess ecGen
      when (ecGen /= ExitSuccess)
        (putStrLn ("ffmpeg png gen stderr: " ++ show (showBS (LBS.toStrict genErr2))))
      (ec, _, rErr) <- runProcessBS "ffmpeg" (resizeArgs png (Just 50) Nothing 90 out)
      check "resizeArgs run (50 wide) on png: exit Success" ExitSuccess ec
      when (ec /= ExitSuccess)
        (putStrLn ("ffmpeg resize stderr: " ++ show (showBS (LBS.toStrict rErr))))
      outExists <- doesFileExist out
      check "resized jpg exists after ffmpeg" True outExists
      mFp <- findExecutable "ffprobe"
      case mFp of
        Nothing -> putStrLn "SKIP: ffprobe not found on PATH -- skipping width assertion"
        Just _ -> do
          (ecP, pOut, _) <- runProcessBS "ffprobe"
            [ "-v", "error", "-select_streams", "v:0"
            , "-show_entries", "stream=width", "-of", "csv=p=0", out ]
          check "ffprobe on resized jpg: exit Success" ExitSuccess ecP
          check "ffprobe reports resized width 50" ["50"] (lines (LBSC.unpack pOut))
      removeFile png
      removeFile out

  -- Items query helpers (milestone 4a, docs/todo.org section 4): the pure
  -- functions of FMS.Core.Query.
  let qp = queryParams
        [ (BSC.pack "search", Just (BSC.pack "a%20b+c"))
        , (BSC.pack "empty", Nothing)
        , (BSC.pack "uni",   Just (BSC.pack "caf%C3%A9"))
        , (BSC.pack "plain", Just (BSC.pack "x"))
        ]
  check "queryParams percent-decodes (%20, + as space, UTF-8) and drops Nothing"
    [("search", "a b c"), ("uni", "café"), ("plain", "x")] qp
  check "queryParams drops valueless params"
    [("a", "1")] (queryParams [(BSC.pack "a", Just (BSC.pack "1")), (BSC.pack "b", Nothing)])
  check "queryParams on empty input" [] (queryParams [])

  -- A sample library of 4 items with distinct titles, sizes and times.
  let t1 = read "2024-01-01 00:00:00 UTC" :: UTCTime
      t2 = read "2024-01-02 00:00:00 UTC" :: UTCTime
      t3 = read "2024-01-03 00:00:00 UTC" :: UTCTime
      t4 = read "2024-01-04 00:00:00 UTC" :: UTCTime
      mkItem :: Int -> T.Text -> Integer -> UTCTime -> MediaItem
      mkItem i title size t = MediaItem
        { itemId      = T.pack (show i)
        , itemTitle   = title
        , itemPath    = "/" ++ T.unpack title ++ ".mp4"
        , itemSize    = size
        , itemMime    = "video/mp4"
        , itemAddedAt = t
        , itemFolder  = "1"
        , itemType      = "Movie"
        , itemSeries    = Nothing
        , itemSeason    = Nothing
        , itemEpisode   = Nothing
        , itemExtraType = Nothing
        }
      items4 =
        [ mkItem 1 "alpha"      300 t3
        , mkItem 2 "beta"       100 t1
        , mkItem 3 "gamma song" 200 t2
        , mkItem 4 "delta"      400 t4
        ]

  -- searchItems: case-insensitive substring match on itemTitle.
  check "searchItems is case-insensitive" ["alpha"] (map itemTitle (searchItems "ALPHA" items4))
  check "searchItems substring (SONG in 'gamma song')" ["gamma song"] (map itemTitle (searchItems "SONG" items4))
  check "searchItems no match -> []" [] (map itemTitle (searchItems "zzz" items4))
  check "searchItems empty term matches all" 4 (length (searchItems "" items4))

  -- sortItems: title asc by default, size desc, addedAt, unknown -> title.
  check "sortItems default is title asc"
    ["alpha", "beta", "delta", "gamma song"] (map itemTitle (sortItems Nothing Nothing items4))
  check "sortItems title desc"
    ["gamma song", "delta", "beta", "alpha"]
    (map itemTitle (sortItems (Just "title") (Just "desc") items4))
  check "sortItems size desc"
    ["delta", "alpha", "gamma song", "beta"]
    (map itemTitle (sortItems (Just "size") (Just "desc") items4))
  check "sortItems size asc when sortOrder missing"
    ["beta", "gamma song", "alpha", "delta"]
    (map itemTitle (sortItems (Just "size") Nothing items4))
  check "sortItems addedAt asc"
    ["beta", "gamma song", "alpha", "delta"]
    (map itemTitle (sortItems (Just "addedAt") (Just "asc") items4))
  check "sortItems unknown sortBy falls back to title"
    ["alpha", "beta", "delta", "gamma song"]
    (map itemTitle (sortItems (Just "bogus") Nothing items4))
  let dup = [mkItem 5 "same" 30 t1, mkItem 6 "same" 10 t2, mkItem 7 "same" 20 t3]
  check "sortItems is stable on equal keys" [30, 10, 20] (map itemSize (sortItems Nothing Nothing dup))

  -- paginate: windows, clamping, defaults.
  let xs5 = [1 .. 5] :: [Int]
  check "paginate with no params returns all" xs5 (paginate Nothing Nothing xs5)
  check "paginate startIndex only" [3, 4, 5] (paginate (Just 2) Nothing xs5)
  check "paginate limit only" [1, 2] (paginate Nothing (Just 2) xs5)
  check "paginate startIndex+limit window" [2] (paginate (Just 1) (Just 1) xs5)
  check "paginate startIndex beyond the end -> []" [] (paginate (Just 10) Nothing xs5)
  check "paginate negative startIndex clamped to 0" [1, 2] (paginate (Just (-3)) (Just 2) xs5)
  check "paginate limit larger than the rest" [4, 5] (paginate (Just 3) (Just 100) xs5)

  -- applyItemQuery: search + sort + paginate combined; the total count is
  -- taken BEFORE pagination.
  let titlesOf (n, is) = (n, map itemTitle is)
  check "applyItemQuery with no params returns all, total 4"
    (4, ["alpha", "beta", "delta", "gamma song"])
    (titlesOf (applyItemQuery [] items4))
  check "applyItemQuery search only (total = number of matches)"
    (2, ["beta", "delta"]) (titlesOf (applyItemQuery [("search", "ta")] items4))
  check "applyItemQuery search + sortBy=size + sortOrder=desc"
    (2, ["delta", "beta"])
    (titlesOf (applyItemQuery [("search", "ta"), ("sortBy", "size"), ("sortOrder", "desc")] items4))
  check "applyItemQuery combined search+sort+paginate keeps pre-pagination total"
    (2, ["beta"])
    (titlesOf (applyItemQuery
        [ ("search", "ta"), ("sortBy", "size"), ("sortOrder", "desc")
        , ("startIndex", "1"), ("limit", "1") ] items4))
  check "applyItemQuery invalid ints are ignored (defaults apply)"
    (4, ["alpha", "beta", "delta", "gamma song"])
    (titlesOf (applyItemQuery [("limit", "abc"), ("startIndex", "xyz")] items4))
  check "applyItemQuery startIndex beyond end -> empty page, total kept"
    (4, []) (titlesOf (applyItemQuery [("startIndex", "99")] items4))

  -- Stats module (milestone 5a, docs/roadmap.org milestone 5): the pure
  -- helpers of FMS.Modules.Stats.Monad on a sample map, the applyPlay /
  -- applyProgress semantics behind recordPlay / recordProgress, and the
  -- JSON persistence roundtrip.
  let pA = Progress 30 120 t1   -- 25%  -> continue watching
      pB = Progress 115 120 t2  -- 95.8% -> finished, excluded
      pC = Progress 5 100 t3    -- 5%   -> continue watching
      statsMap = HM.fromList
        [ ("a", ItemStats 3 t4 (Just pA))
        , ("b", ItemStats 1 t2 (Just pB))
        , ("c", ItemStats 5 t3 (Just pC))
        , ("d", ItemStats 2 t1 Nothing)
        ]
  check "continueWatching keeps unfinished progress, updatedAt desc"
    [("c", pC), ("a", pA)] (continueWatching statsMap)
  check "continueWatching excludes >=95% and progress-less items"
    2 (length (continueWatching statsMap))
  check "popular sorts by playCount desc"
    ["c", "a", "d", "b"] (map fst (popular statsMap))
  check "popular keeps the full ItemStats values"
    [5, 3, 2, 1] (map (statsPlayCount . snd) (popular statsMap))

  let s0 = ItemStats 0 t1 Nothing
  check "applyPlay bumps playCount and stamps lastPlayedAt"
    (ItemStats 1 t4 Nothing) (applyPlay t4 s0)
  check "applyPlay twice bumps twice"
    (ItemStats 2 t4 Nothing) (applyPlay t4 (applyPlay t4 s0))
  check "applyProgress sets progress, keeps playCount"
    (ItemStats 0 t1 (Just pA)) (applyProgress pA s0)
  check "applyProgress overwrites existing progress"
    (ItemStats 3 t4 (Just pC)) (applyProgress pC (ItemStats 3 t4 (Just pA)))

  -- JSON persistence: saveStats/loadStats roundtrip in /tmp.
  let statsFile = "/tmp/fms-test-stats.json"
  saveStats statsFile statsMap
  loadedStats <- loadStats statsFile
  check "saveStats/loadStats roundtrip" statsMap loadedStats
  rawStats <- BSC.readFile statsFile
  check "stats.json is a bare map containing the item ids"
    True ("\"a\"" `isInfixOf` BSC.unpack rawStats
          && "\"c\"" `isInfixOf` BSC.unpack rawStats
          && "\"playCount\"" `isInfixOf` BSC.unpack rawStats)
  missingStats <- loadStats "/tmp/fms-test-stats-does-not-exist.json"
  check "loadStats of a missing file is empty" HM.empty missingStats
  removeFile statsFile

  -- Auth module (milestone 5b, docs/todo.org section 8): the router's pure
  -- helpers (open paths + token header parsing), the bcrypt roundtrip, the
  -- users.json persistence roundtrip, and the public User JSON shape.

  -- isOpenPath: only the exact open paths are open.
  check "isOpenPath System/Info/Public" True (isOpenPath ["System", "Info", "Public"])
  check "isOpenPath healthz" True (isOpenPath ["healthz"])
  check "isOpenPath Users/AuthenticateByName" True (isOpenPath ["Users", "AuthenticateByName"])
  check "isOpenPath Users/Public" True (isOpenPath ["Users", "Public"])
  check "isOpenPath Users is False" False (isOpenPath ["Users"])
  check "isOpenPath Users/1 is False" False (isOpenPath ["Users", "1"])
  check "isOpenPath System/Info is False" False (isOpenPath ["System", "Info"])
  check "isOpenPath library is False" False (isOpenPath ["library"])
  check "isOpenPath [] is False" False (isOpenPath [])

  -- parseTokenHeader: X-Emby-Token, MediaBrowser Token="...", Bearer, and
  -- the negative cases.
  check "parseTokenHeader X-Emby-Token"
    (Just "tok1") (parseTokenHeader [(BSC.pack "X-Emby-Token", BSC.pack "tok1")])
  check "parseTokenHeader header name is case-insensitive"
    (Just "tok1") (parseTokenHeader [(BSC.pack "x-emby-token", BSC.pack "tok1")])
  check "parseTokenHeader MediaBrowser Token=\"...\""
    (Just "abc123") (parseTokenHeader [(BSC.pack "Authorization", BSC.pack "MediaBrowser Token=\"abc123\"")])
  check "parseTokenHeader MediaBrowser with other params first"
    (Just "t9") (parseTokenHeader [(BSC.pack "Authorization", BSC.pack "MediaBrowser Client=\"Web\", Device=\"x\", Token=\"t9\"")])
  check "parseTokenHeader Bearer"
    (Just "bt") (parseTokenHeader [(BSC.pack "Authorization", BSC.pack "Bearer bt")])
  check "parseTokenHeader bearer (lowercase scheme)"
    (Just "bt") (parseTokenHeader [(BSC.pack "Authorization", BSC.pack "bearer bt")])
  check "parseTokenHeader missing headers -> Nothing" Nothing (parseTokenHeader [])
  check "parseTokenHeader Basic auth -> Nothing"
    Nothing (parseTokenHeader [(BSC.pack "Authorization", BSC.pack "Basic dXNlcjpwYXNz")])
  check "parseTokenHeader empty X-Emby-Token -> Nothing"
    Nothing (parseTokenHeader [(BSC.pack "X-Emby-Token", BSC.pack "")])
  check "parseTokenHeader unrelated headers -> Nothing"
    Nothing (parseTokenHeader [(BSC.pack "Host", BSC.pack "localhost:8117")])

  -- bcrypt roundtrip. Cost-14 hashing is slowish (~1s per op here), so keep
  -- the op count at three: one hash + two verifies.
  h <- hashPassword "s3cret-pw"
  check "bcrypt hash has a $2y$ prefix" True ("$2y$" `T.isPrefixOf` h)
  check "bcrypt verify accepts the right password" True (verifyPassword h "s3cret-pw")
  check "bcrypt verify rejects a wrong password" False (verifyPassword h "nope")

  -- users.json persistence roundtrip (internal JSON keeps the hash).
  let ufile = "/tmp/fms-test-users.json"
      u1 = User "1" "admin" "$2y$14$somehash" True ["key1"] Nothing
      u2 = User "2" "bob" "hash2" False [] (Just ["1"])
      usersMap = HM.fromList [("1", u1), ("2", u2)]
  saveUsers ufile usersMap
  loadedUsers <- loadUsers ufile
  check "saveUsers/loadUsers roundtrip" usersMap loadedUsers
  rawUsers <- BSC.readFile ufile
  check "users.json is a bare map and keeps passwordHash internally"
    True ("passwordHash" `BSC.isInfixOf` rawUsers
          && "\"1\"" `BSC.isInfixOf` rawUsers
          && "\"2\"" `BSC.isInfixOf` rawUsers)
  missingUsers <- loadUsers "/tmp/fms-test-users-does-not-exist.json"
  check "loadUsers of a missing file is empty" HM.empty missingUsers
  removeFile ufile

  -- User JSON shapes: ToJSON is the public one (no passwordHash, has
  -- hasPassword); FromJSON/toInternalJSON are the internal full shape.
  let uj1 = LBS.toStrict (Aeson.encode u1)
      uj3 = LBS.toStrict (Aeson.encode (User "3" "nopw" "" False [] Nothing))
  check "User ToJSON omits passwordHash" False ("passwordHash" `BSC.isInfixOf` uj1)
  check "User ToJSON has hasPassword:true when a hash is set"
    True ("\"hasPassword\":true" `BSC.isInfixOf` uj1)
  check "User ToJSON has hasPassword:false when the hash is empty"
    True ("\"hasPassword\":false" `BSC.isInfixOf` uj3)
  check "User ToJSON carries id/name/isAdmin"
    True (all (`BSC.isInfixOf` uj1) ["\"id\":\"1\"", "\"name\":\"admin\"", "\"isAdmin\":true"])
  check "User FromJSON parses the internal full shape"
    (Just u1) (Aeson.decode (Aeson.encode (toInternalJSON u1)) :: Maybe User)

  -- Milestone 5d (per-folder permissions): canAccessFolder matrix, old
  -- users.json compat (missing allowedFolders -> Nothing), and the
  -- permissions request JSON roundtrip.
  check "canAccessFolder Nothing allows everything" True (canAccessFolder Nothing "2")
  check "canAccessFolder (Just ids) allows a listed folder" True (canAccessFolder (Just ["1", "3"]) "3")
  check "canAccessFolder (Just ids) denies an unlisted folder" False (canAccessFolder (Just ["1"]) "2")
  check "canAccessFolder (Just []) denies everything" False (canAccessFolder (Just []) "1")
  let oldUserJson = "{\"userId\":\"1\",\"userName\":\"admin\",\"passwordHash\":\"h\",\"isAdmin\":true,\"apiKeys\":[]}"
  check "User FromJSON without allowedFolders (pre-5d file) defaults to Nothing"
    (Just Nothing) (userAllowedFolders <$> (Aeson.decode oldUserJson :: Maybe User))
  check "User FromJSON with allowedFolders:[\"1\"] parses to Just"
    (Just (Just ["1"]))
    (userAllowedFolders <$> (Aeson.decode "{\"userId\":\"2\",\"userName\":\"b\",\"passwordHash\":\"h\",\"isAdmin\":false,\"apiKeys\":[],\"allowedFolders\":[\"1\"]}" :: Maybe User))
  check "User FromJSON with allowedFolders:null decodes to Nothing"
    (Just Nothing)
    (userAllowedFolders <$> (Aeson.decode "{\"userId\":\"2\",\"userName\":\"b\",\"passwordHash\":\"h\",\"isAdmin\":false,\"apiKeys\":[],\"allowedFolders\":null}" :: Maybe User))
  check "User public ToJSON carries AllowedFolders"
    True ("\"AllowedFolders\":[\"1\"]" `BSC.isInfixOf` LBS.toStrict (Aeson.encode u2))
  check "User internal JSON persists allowedFolders"
    True ("\"allowedFolders\":[\"1\"]" `BSC.isInfixOf` LBS.toStrict (Aeson.encode (toInternalJSON u2)))
  let prq = PermissionsRequest (Just ["1", "2"])
  check "PermissionsRequest JSON roundtrip"
    (Just prq) (Aeson.decode (Aeson.encode prq) :: Maybe PermissionsRequest)
  check "PermissionsRequest decodes null AllowedFolders as Nothing"
    (Just (PermissionsRequest Nothing))
    (Aeson.decode "{\"AllowedFolders\":null}" :: Maybe PermissionsRequest)
  check "PermissionsRequest decodes a missing key as Nothing"
    (Just (PermissionsRequest Nothing))
    (Aeson.decode "{}" :: Maybe PermissionsRequest)

  -- Rate limiting + request ids (milestone 14a, docs/todo.org section 14):
  -- the pure token bucket math and the request-id format.
  let r = 60.0 :: Double   -- 60 req/min = 1 token/s, cap 60
      b0 = newBucket r tNow
  check "newBucket starts full at cap = rate" 60 (bucketTokens b0)
  let (ok1, tb1) = tryTake r tNow b0
  check "tryTake on a full bucket succeeds" True ok1
  check "tryTake consumes exactly one token" 59 (bucketTokens tb1)
  let empty = Bucket 0 tNow
      (ok2, tb2) = tryTake r tNow empty
  check "tryTake on an empty bucket denies" False ok2
  check "a denied tryTake does not go negative" 0 (bucketTokens tb2)
  check "refill adds rate/60 * elapsed tokens"
    10 (bucketTokens (refill r (addUTCTime 10 tNow) empty))
  check "refill caps at rate (never exceeds cap)"
    60 (bucketTokens (refill r (addUTCTime 3600 tNow) empty))
  let (ok3, tb3) = tryTake r (addUTCTime 1 tNow) empty
  check "tryTake refills first: 1s elapsed -> 1 token -> allowed"
    (True, 0) (ok3, bucketTokens tb3)
  check "requestIdHex 0 is 16 zero chars" "0000000000000000" (requestIdHex 0)
  check "requestIdHex 255 is zero-padded 16-char hex" "00000000000000ff" (requestIdHex 255)
  check "requestIdHex 65536 keeps 16 chars" 16 (T.length (requestIdHex 65536))

  -- ApiDocs module (milestone 1c, docs/todo.org section 1): the pure
  -- route -> OpenAPI path mapping and the spec generator.
  check "segmentToPathPiece Lit" "items" (segmentToPathPiece (Lit "items"))
  check "segmentToPathPiece Capture" "{id}" (segmentToPathPiece (Capture "id"))
  check "methodText lowercases the method" "get" (methodText "GET")
  let rDocs = get [Lit "items", Capture "id", Lit "metadata"] undefined
  check "routeToPath mixed Lit/Capture pattern" "/items/{id}/metadata" (routeToPath rDocs)
  check "routeToPath of the empty (root) pattern is /" "/" (routeToPath (get [] undefined))
  let spec     = routesToOpenApi [("fake", [rDocs, post [Lit "items"] undefined])]
      specTxt  = LBSC.unpack (Aeson.encode spec)
  check "generated spec declares openapi 3.0.3"
    True ("\"openapi\":\"3.0.3\"" `isInfixOf` specTxt)
  check "spec info has the server title/version"
    True ("\"title\":\"functional-media-server\"" `isInfixOf` specTxt
          && "\"version\":\"0.1.0.0\"" `isInfixOf` specTxt)
  check "spec paths contain /items/{id}/metadata"
    True ("\"/items/{id}/metadata\"" `isInfixOf` specTxt)
  check "spec path object has get (and /items has post)"
    True ("\"get\"" `isInfixOf` specTxt && "\"/items\":" `isInfixOf` specTxt
          && "\"post\"" `isInfixOf` specTxt)
  check "capture id becomes a required path parameter"
    True (all (`isInfixOf` specTxt)
      ["\"name\":\"id\"", "\"in\":\"path\"", "\"required\":true", "\"type\":\"string\""])
  check "operation has tags with the module name and a 200 response"
    True ("\"tags\":[\"fake\"]" `isInfixOf` specTxt && "\"200\"" `isInfixOf` specTxt)

  -- Metadata module (milestone 4b, docs/roadmap.org milestone 4,
  -- docs/todo.org section 3): the pure NFO parser, nfoPathFor, and the
  -- metadata.json persistence roundtrip.
  let sampleNfo = BSC.pack (unlines
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        , "<movie>"
        , "  <title>Test Movie</title>"
        , "  <originaltitle>Original Test Movie</originaltitle>"
        , "  <year>2020</year>"
        , "  <rating>7.5</rating>"
        , "  <runtime>95</runtime>"
        , "  <genre>Action</genre>"
        , "  <genre>Sci-Fi</genre>"
        , "  <overview>A test overview.</overview>"
        , "  <tmdbid>42</tmdbid>"
        , "  <imdbid>tt1234567</imdbid>"
        , "</movie>"
        ])
      expectedMeta = MediaMetadata
        { metaTitle          = Just "Test Movie"
        , metaOverview       = Just "A test overview."
        , metaGenres         = ["Action", "Sci-Fi"]
        , metaStudios        = []
        , metaPersons        = []
        , metaTags           = []
        , metaYear           = Just 2020
        , metaRating         = Just 7.5
        , metaRuntimeMinutes = Just 95
        , metaProviderIds    = HM.fromList [("tmdb", "42"), ("imdb", "tt1234567")]
        , metaSource         = "nfo"
        }
      isLeftE' :: Either String MediaMetadata -> Bool
      isLeftE' (Left _)  = True
      isLeftE' (Right _) = False
  check "parseMovieNfo parses a full <movie> NFO"
    (Right expectedMeta) (parseMovieNfo sampleNfo)
  check "parseMovieNfo on malformed XML is Left"
    True (isLeftE' (parseMovieNfo (BSC.pack "<movie><title>oops")))
  check "parseMovieNfo on non-movie XML is Left"
    True (isLeftE' (parseMovieNfo (BSC.pack "<tvshow><title>x</title></tvshow>")))
  let sparseMeta = MediaMetadata
        (Just "Only Title") Nothing [] [] [] [] Nothing Nothing Nothing HM.empty "nfo"
  check "parseMovieNfo with missing fields -> Nothing/empty fields"
    (Right sparseMeta)
    (parseMovieNfo (BSC.pack "<movie><title>Only Title</title></movie>"))
  check "parseMovieNfo ignores unparseable numbers"
    (Right (sparseMeta { metaTitle = Just "T", metaYear = Nothing }))
    (parseMovieNfo (BSC.pack "<movie><title>T</title><year>abc</year></movie>"))
  check "nfoPathFor replaces the extension"
    "/media/clip.nfo" (nfoPathFor "/media/clip.mp4")
  check "nfoPathFor keeps the directory"
    "/a/b/song.nfo" (nfoPathFor "/a/b/song.mp3")

  -- metadata.json persistence: saveMetadata/loadMetadata roundtrip in /tmp.
  let metaFile = "/tmp/fms-test-metadata.json"
      metaMap = HM.fromList [("1", expectedMeta), ("2", sparseMeta)]
  saveMetadata metaFile metaMap
  loadedMeta <- loadMetadata metaFile
  check "saveMetadata/loadMetadata roundtrip" metaMap loadedMeta
  rawMeta <- BSC.readFile metaFile
  check "metadata.json is a bare map containing ids and providerIds"
    True ("\"1\"" `isInfixOf` BSC.unpack rawMeta
          && "\"tmdb\"" `isInfixOf` BSC.unpack rawMeta
          && "\"providerIds\"" `isInfixOf` BSC.unpack rawMeta
          && "\"source\":\"nfo\"" `isInfixOf` BSC.unpack rawMeta)
  missingMeta <- loadMetadata "/tmp/fms-test-metadata-does-not-exist.json"
  check "loadMetadata of a missing file is empty" HM.empty missingMeta
  removeFile metaFile

  -- TMDB remote provider (milestone 4c, docs/roadmap.org milestone 4,
  -- docs/todo.org section 3): the PURE helpers only — no network in tests.
  let tmdbUrl = tmdbSearchUrl "TESTKEY" "a b"
  check "tmdbSearchUrl is the /search/movie endpoint"
    True ("https://api.themoviedb.org/3/search/movie?" `isPrefixOf` tmdbUrl)
  check "tmdbSearchUrl contains api_key=TESTKEY"
    True ("api_key=TESTKEY" `isInfixOf` tmdbUrl)
  check "tmdbSearchUrl encodes the query ('a b' -> a%20b or a+b)"
    True ("a%20b" `isInfixOf` tmdbUrl || "a+b" `isInfixOf` tmdbUrl)
  let tmdbUrlUni = tmdbSearchUrl "K" "café"
  check "tmdbSearchUrl UTF-8 percent-encodes non-ASCII (café -> caf%C3%A9)"
    True ("caf%C3%A9" `isInfixOf` tmdbUrlUni)

  -- A 2-result /search/movie fixture: full first hit, second hit without
  -- overview. release_date must yield the year; a missing overview -> Nothing.
  let tmdbFixture = LBS.fromStrict (BSC.pack (unlines
        [ "{\"page\":1,\"total_results\":2,\"total_pages\":1,\"results\":["
        , "  {\"id\":550,\"title\":\"Fight Club\",\"original_title\":\"Fight Club\",\"overview\":\"An insomniac office worker.\",\"release_date\":\"1999-10-15\",\"vote_average\":8.433,\"vote_count\":26280},"
        , "  {\"id\":680,\"title\":\"Pulp Fiction\",\"release_date\":\"1994-09-10\",\"vote_average\":8.5}"
        , "]}"
        ]))
      tmdbExpected =
        [ TmdbResult 550 "Fight Club" (Just "An insomniac office worker.") (Just 1999) (Just 8.433)
        , TmdbResult 680 "Pulp Fiction" Nothing (Just 1994) (Just 8.5)
        ]
  check "parseTmdbResults parses a 2-result fixture (year extraction, missing overview -> Nothing)"
    (Right tmdbExpected) (parseTmdbResults tmdbFixture)
  check "parseTmdbResults on an empty results array is Right []"
    (Right []) (parseTmdbResults (LBS.fromStrict (BSC.pack "{\"results\":[]}")))
  check "parseTmdbResults: empty release_date -> year Nothing, vote_average 0 -> Just 0.0"
    (Right [TmdbResult 7 "X" Nothing Nothing (Just 0.0)])
    (parseTmdbResults (LBS.fromStrict (BSC.pack
      "{\"results\":[{\"id\":7,\"title\":\"X\",\"release_date\":\"\",\"vote_average\":0}]}")))
  check "parseTmdbResults: malformed release_date -> year Nothing"
    (Right [TmdbResult 8 "Y" Nothing Nothing (Just 7.0)])
    (parseTmdbResults (LBS.fromStrict (BSC.pack
      "{\"results\":[{\"id\":8,\"title\":\"Y\",\"release_date\":\"soon\",\"vote_average\":7}]}")))
  let isLeftStr :: Either String a -> Bool
      isLeftStr (Left _)  = True
      isLeftStr (Right _) = False
  check "parseTmdbResults on invalid JSON is Left"
    True (isLeftStr (parseTmdbResults (LBS.fromStrict (BSC.pack "not json"))))
  check "parseTmdbResults on a non-object body is Left"
    True (isLeftStr (parseTmdbResults (LBS.fromStrict (BSC.pack "[1,2]"))))
  check "parseTmdbResults without a results key is Left"
    True (isLeftStr (parseTmdbResults (LBS.fromStrict (BSC.pack "{\"page\":1}"))))

  -- UserData module (milestone 5c, docs/todo.org section 9): the pure
  -- helpers of FMS.Modules.UserData.Monad, the UserData JSON roundtrip
  -- (incl. partial decode), and the nested userdata.json persistence
  -- roundtrip in /tmp.
  let ud0 = emptyUserData
  check "emptyUserData is all-empty"
    (UserData False False Nothing 0 Nothing) ud0
  check "markPlayed True sets played and bumps playCount"
    (UserData True False Nothing 1 Nothing) (markPlayed True ud0)
  check "markPlayed True twice bumps the play count twice"
    (UserData True False Nothing 2 Nothing) (markPlayed True (markPlayed True ud0))
  check "markPlayed False clears played, keeps playCount"
    (UserData False False Nothing 2 Nothing) (markPlayed False (markPlayed True (markPlayed True ud0)))
  check "setFavorite True sets favorite"
    (ud0 { udFavorite = True }) (setFavorite True ud0)
  check "setFavorite False clears favorite"
    (ud0 { udFavorite = False }) (setFavorite False (setFavorite True ud0))
  check "setPosition sets playbackPosition"
    (ud0 { udPlaybackPosition = Just 42.5 }) (setPosition 42.5 ud0)
  check "setPosition overwrites an existing position"
    (Just 99.0) (udPlaybackPosition (setPosition 99.0 (setPosition 42.5 ud0)))

  -- UserData JSON: roundtrip + Jellyfin-style keys + partial decode.
  let udSample = UserData True True (Just 42.5) 3 (Just t4)
      udJson   = LBS.toStrict (Aeson.encode udSample)
  check "UserData JSON roundtrip" (Just udSample) (Aeson.decode (Aeson.encode udSample) :: Maybe UserData)
  check "UserData ToJSON uses the Jellyfin-style keys"
    True (all (`BSC.isInfixOf` udJson)
      [ "\"played\":true", "\"favorite\":true", "\"playbackPosition\":42.5"
      , "\"playCount\":3", "\"lastPlayedAt\":" ])
  check "UserData empty encodes null position/lastPlayedAt and 0 playCount"
    True (all (`BSC.isInfixOf` LBS.toStrict (Aeson.encode ud0))
      [ "\"played\":false", "\"favorite\":false", "\"playbackPosition\":null"
      , "\"playCount\":0", "\"lastPlayedAt\":null" ])
  check "UserData partial decode {\"played\":true} keeps the other defaults"
    (Just (ud0 { udPlayed = True })) (Aeson.decode "{\"played\":true}" :: Maybe UserData)
  check "UserData partial decode {} decodes to emptyUserData"
    (Just ud0) (Aeson.decode "{}" :: Maybe UserData)
  check "UserData partial decode {\"playbackPosition\":7.25}"
    (Just (ud0 { udPlaybackPosition = Just 7.25 }))
    (Aeson.decode "{\"playbackPosition\":7.25}" :: Maybe UserData)
  check "UserData decode of a non-object is Nothing"
    (Nothing :: Maybe UserData) (Aeson.decode "[1,2]")
  check "UserData decode of a wrongly typed field is Nothing"
    (Nothing :: Maybe UserData) (Aeson.decode "{\"played\":\"yes\"}")

  -- userdata.json persistence: nested map userId -> itemId -> UserData.
  let udFile = "/tmp/fms-test-userdata.json"
      udMap = HM.fromList
        [ ("1", HM.fromList [("10", udSample), ("11", ud0)])
        , ("2", HM.fromList [("10", ud0 { udFavorite = True })])
        ]
  saveUserData udFile udMap
  loadedUD <- loadUserData udFile
  check "saveUserData/loadUserData nested roundtrip" udMap loadedUD
  rawUD <- BSC.readFile udFile
  check "userdata.json is a nested map (user -> item -> UserData)"
    True ("\"1\"" `BSC.isInfixOf` rawUD
          && "\"2\"" `BSC.isInfixOf` rawUD
          && "\"10\"" `BSC.isInfixOf` rawUD
          && "\"played\":true" `BSC.isInfixOf` rawUD
          && "\"playbackPosition\":42.5" `BSC.isInfixOf` rawUD)
  missingUD <- loadUserData "/tmp/fms-test-userdata-does-not-exist.json"
  check "loadUserData of a missing file is empty" HM.empty missingUD
  removeFile udFile

  -- Playback module (milestone 6a, docs/todo.org section 6): the pure
  -- ffprobe argument list, the pure parseFfprobe parser on a fixture,
  -- the container-from-extension fallback, and the direct-play /
  -- direct-stream / transcode decision matrix.
  check "probeArgs is the ffprobe json show_format/show_streams invocation"
    ["-v","error","-print_format","json","-show_format","-show_streams","/x.mp4"]
    (probeArgs "/x.mp4")

  let ffprobeFixture = LBS.fromStrict (BSC.pack (unwords
        [ "{"
        , "\"streams\":["
        , "{\"index\":0,\"codec_name\":\"h264\",\"codec_type\":\"video\",\"width\":1920,\"height\":1080,\"duration\":\"61.000000\",\"tags\":{\"language\":\"eng\"}},"
        , "{\"index\":1,\"codec_name\":\"aac\",\"codec_type\":\"audio\",\"channels\":2,\"tags\":{\"language\":\"eng\"}},"
        , "{\"index\":2,\"codec_name\":\"srt\",\"codec_type\":\"subtitle\",\"tags\":{\"language\":\"fre\",\"title\":\"Forced\"}}"
        , "],"
        , "\"format\":{\"format_name\":\"mov,mp4,m4a,3gp,3g2,mj2\",\"duration\":\"120.500000\"}"
        , "}"
        ]))
      isRightE :: Either a b -> Bool
      isRightE (Right _) = True
      isRightE (Left _)  = False
  case parseFfprobe ffprobeFixture of
    Left err -> do
      putStrLn ("FAIL: parseFfprobe fixture -- " ++ err)
      exitFailure
    Right (container, streams) -> do
      check "parseFfprobe fixture container is mp4 (mov,mp4,... -> mp4)"
        "mp4" (T.unpack container)
      check "parseFfprobe fixture has 3 streams" 3 (length streams)
      check "parseFfprobe stream types" ["Video","Audio","Subtitle"] (map msType streams)
      check "parseFfprobe stream codecs" ["h264","aac","srt"] (map msCodec streams)
      check "parseFfprobe stream indexes" [0,1,2] (map msIndex streams)
      check "parseFfprobe video width/height"
        (Just 1920, Just 1080) (msWidth (streams !! 0), msHeight (streams !! 0))
      check "parseFfprobe audio channels + language"
        (Just 2, Just "eng") (msChannels (streams !! 1), msLanguage (streams !! 1))
      check "parseFfprobe subtitle language + title"
        (Just "fre", Just "Forced") (msLanguage (streams !! 2), msTitle (streams !! 2))
      check "parseFfprobe stream duration (stream's own)"
        (Just 61.0) (msDurationSeconds (streams !! 0))
      check "parseFfprobe falls back to format duration"
        (Just 120.5) (msDurationSeconds (streams !! 1))
  check "parseFfprobe on malformed JSON is Left"
    True (isLeftE (parseFfprobe "this is not json at all"))
  check "parseFfprobe on wrong-typed streams is Left"
    True (isLeftE (parseFfprobe "{\"streams\": 42}"))
  check "parseFfprobe on a non-object is Left"
    True (isLeftE (parseFfprobe "[1,2,3]"))

  check "containerFromExtension lowercases .MP4" "mp4" (T.unpack (containerFromExtension "/a/b/clip.MP4"))
  check "containerFromExtension no extension -> unknown" "unknown" (T.unpack (containerFromExtension "/a/b/noext"))

  -- The decision matrix (default client profile).
  let vcodec c = MediaStream 0 "Video"   c Nothing Nothing Nothing Nothing Nothing Nothing
      acodec c = MediaStream 1 "Audio"   c Nothing Nothing Nothing Nothing Nothing Nothing
      sub    c = MediaStream 2 "Subtitle" c Nothing Nothing Nothing Nothing Nothing Nothing
  check "directPlayable mp4 h264+aac is True"
    True (directPlayable "mp4" [vcodec "h264", acodec "aac"])
  check "directPlayable mp4 hevc+ac3 is False (ac3 not in the audio list)"
    False (directPlayable "mp4" [vcodec "hevc", acodec "ac3"])
  check "directPlayable mkv h264+aac is False (container not direct-play)"
    False (directPlayable "mkv" [vcodec "h264", acodec "aac"])
  check "directStreamable mkv is True (remux)"
    True (directStreamable "mkv" [vcodec "h264", acodec "aac"])
  check "transcoding mkv is True (not directPlayable)"
    True (transcoding "mkv" [vcodec "h264", acodec "aac"])
  check "directPlayable avi mpeg4+mp3 is False -> transcode"
    False (directPlayable "avi" [vcodec "mpeg4", acodec "mp3"])
  check "transcoding avi is True"
    True (transcoding "avi" [vcodec "mpeg4", acodec "mp3"])
  check "directPlayable mp3 audio-only is True"
    True (directPlayable "mp3" [acodec "mp3"])
  check "directStreamable mp4 is False"
    False (directStreamable "mp4" [vcodec "h264"])
  check "directPlayable ignores subtitle streams"
    True (directPlayable "mp4" [vcodec "h264", acodec "aac", sub "srt"])
  check "directPlayable webm vp9+opus is True"
    True (directPlayable "webm" [vcodec "vp9", acodec "opus"])
  check "directPlayable mp4 with unknown video codec is False"
    False (directPlayable "mp4" [vcodec "wmv3", acodec "aac"])

  -- IO test of the real ffprobe round trip, guarded by ffprobe/ffmpeg
  -- availability: skip gracefully when they are not on PATH.
  mFfprobe <- findExecutable "ffprobe"
  case (mFfprobe, mFFmpeg) of
    (Just _, Just _) -> do
      let clip  = "/tmp/fms-test-probe-clip.mp4"
          dummy = "/tmp/fms-test-probe-dummy.mp4"
      (ecGen, _, genErr) <- runProcessBS "ffmpeg"
        [ "-f", "lavfi", "-i", "testsrc=duration=2:size=320x240:rate=10"
        , "-f", "lavfi", "-i", "sine=frequency=440:duration=2"
        , "-c:v", "libx264", "-c:a", "aac", "-y", clip ]
      check "generate 2s test clip (video+audio): exit Success" ExitSuccess ecGen
      when (ecGen /= ExitSuccess)
        (putStrLn ("ffmpeg clip gen stderr: " ++ show (showBS (LBS.toStrict genErr))))
      writeFile dummy "not really an mp4, just bytes"
      rClip <- probeFile clip
      check "probeFile on a real clip is Right" True (isRightE rClip)
      case rClip of
        Left err -> do
          putStrLn ("FAIL: probeFile clip -- " ++ err)
          exitFailure
        Right (c, streams) -> do
          check "probeFile clip container is mp4" "mp4" (T.unpack c)
          check "probeFile clip stream types" ["Video", "Audio"] (map msType streams)
          check "probeFile clip codecs" ["h264", "aac"] (map msCodec streams)
          check "probeFile clip is direct playable" True (directPlayable c streams)
          check "probeFile clip is not direct streamable" False (directStreamable c streams)
      rDummy <- probeFile dummy
      check "probeFile on dummy bytes is Left" True (isLeftE rDummy)
      removeFile clip
      removeFile dummy
    _ -> putStrLn "SKIP: ffprobe/ffmpeg not found on PATH -- skipping probe IO test"

  -- Sessions module (milestone 6c, docs/todo.org section 6): the pure
  -- session-map helpers (upsert/expire) and the PlaySession JSON roundtrip.
  let t0 = read "2026-01-01 00:00:00 UTC" :: UTCTime
      s1 = PlaySession "s1" "1" (Just "1") (Just "admin") 0 False t0 t0
      s2 = PlaySession "s2" "2" Nothing Nothing 12.5 True t0 (addUTCTime (-120) t0)
      s3 = s1 { psId = "s3", psLastPingAt = addUTCTime (-90) t0 }
      sessMap = HM.fromList [("s1", s1), ("s2", s2)]
  check "upsertSession inserts a new session"
    (Just s1) (HM.lookup "s1" (upsertSession s1 HM.empty))
  check "upsertSession replaces an existing session in place"
    (Just 5.0)
    (psPositionSeconds <$> HM.lookup "s1" (upsertSession (s1 { psPositionSeconds = 5 }) (upsertSession s1 HM.empty)))
  check "expireSessions drops sessions with last ping older than 90s"
    ["s1"] (sort (HM.keys (expireSessions t0 90 sessMap)))
  check "expireSessions keeps a session exactly at the 90s limit"
    ["s1", "s3"] (sort (HM.keys (expireSessions t0 90 (HM.insert "s3" s3 sessMap))))
  check "PlaySession JSON roundtrip"
    (Just s1) (Aeson.decode (Aeson.encode s1) :: Maybe PlaySession)
  check "PlaySession JSON uses the PascalCase keys"
    True (all (`BSC.isInfixOf` LBS.toStrict (Aeson.encode s1))
      [ "\"Id\":", "\"ItemId\":", "\"UserId\":", "\"UserName\":"
      , "\"PositionSeconds\":", "\"IsPaused\":", "\"StartedAt\":", "\"LastPingAt\":" ])

  -- Genres + Playlists (milestone 10a, docs/todo.org section 10): the
  -- pure genre grouping, the pure playlist item helpers, the Playlist
  -- JSON roundtrip, and the playlists.json persistence roundtrip.
  let gm gs = sparseMeta { metaGenres = gs }
  check "genresFromMetadata groups, counts per item, sorts by name"
    [("Action",1),("Drama",2),("Sci-Fi",1)]
    (genresFromMetadata [gm ["Drama","Action"], gm ["Drama","Sci-Fi"], gm []])
  check "genresFromMetadata counts a duplicate genre in one item once"
    [("Action",1)] (genresFromMetadata [gm ["Action","Action"]])
  check "genresFromMetadata on empty input is []"
    [] (genresFromMetadata [])
  check "addItem appends a new item" ["1","2"] (addItem "2" ["1"])
  check "addItem dedupes an existing item" ["1","2"] (addItem "1" ["1","2"])
  check "removeItem removes all occurrences" ["1"] (removeItem "2" ["1","2","2"])
  check "removeItem of an absent item is a no-op" ["1"] (removeItem "x" ["1"])
  let pl0 = Playlist "p1" "Favs" ["1","2"] t4
      plMap = HM.fromList [("p1", pl0), ("p2", pl0 { plId = "p2", plName = "Other", plItemIds = [] })]
      plFile = "/tmp/fms-test-playlists.json"
  check "Playlist JSON roundtrip" (Just pl0) (Aeson.decode (Aeson.encode pl0) :: Maybe Playlist)
  check "Playlist JSON uses the PascalCase keys"
    True (all (`BSC.isInfixOf` LBS.toStrict (Aeson.encode pl0))
      [ "\"Id\":", "\"Name\":", "\"ItemIds\":", "\"CreatedAt\":" ])
  savePlaylists plFile plMap
  loadedPl <- loadPlaylists plFile
  check "savePlaylists/loadPlaylists roundtrip" plMap loadedPl
  missingPl <- loadPlaylists "/tmp/fms-test-playlists-does-not-exist.json"
  check "loadPlaylists of a missing file is empty" HM.empty missingPl
  removeFile plFile

  -- ServerConfig (milestone 1b, docs/todo.org section 1): defaults, JSON
  -- roundtrip, partial patch decode + merge semantics, file load/save.
  check "defaultServerConfig values"
    ("functional-media-server", False, "./media", 8080)
    ( scServerName defaultServerConfig, scRequireAuth defaultServerConfig
    , scLibraryPath defaultServerConfig, scPort defaultServerConfig )
  check "ServerConfig JSON roundtrip"
    (Just defaultServerConfig)
    (Aeson.decode (Aeson.encode defaultServerConfig) :: Maybe ServerConfig)
  check "ServerConfigPatch partial decode: only ServerName set"
    (Just (ServerConfigPatch (Just "My Media") Nothing Nothing Nothing))
    (Aeson.decode "{\"ServerName\":\"My Media\"}" :: Maybe ServerConfigPatch)
  check "ServerConfigPatch empty object decodes to all-Nothing"
    (Just (ServerConfigPatch Nothing Nothing Nothing Nothing))
    (Aeson.decode "{}" :: Maybe ServerConfigPatch)
  let patch = fromMaybe (ServerConfigPatch Nothing Nothing Nothing Nothing)
            (Aeson.decode "{\"ServerName\":\"My Media\",\"Port\":9999}" :: Maybe ServerConfigPatch)
      merged = applyServerConfigPatch defaultServerConfig patch
  check "applyServerConfigPatch overrides set fields, keeps the rest"
    ("My Media", False, "./media", 9999)
    (scServerName merged, scRequireAuth merged, scLibraryPath merged, scPort merged)
  check "mergeServerConfig: the second argument's fields win"
    (9090, True, "x")
    ( let m = mergeServerConfig defaultServerConfig
                (defaultServerConfig { scPort = 9090, scRequireAuth = True, scServerName = "x" })
      in (scPort m, scRequireAuth m, scServerName m) )
  let cfgFile = "/tmp/fms-test-serverconfig.json"
  missingCfg <- loadServerConfig "/tmp/fms-test-serverconfig-does-not-exist.json"
  check "loadServerConfig of a missing file -> defaults" defaultServerConfig missingCfg
  saveServerConfig cfgFile (defaultServerConfig { scServerName = "Saved", scPort = 1234 })
  loadedCfg <- loadServerConfig cfgFile
  check "saveServerConfig/loadServerConfig roundtrip"
    ("Saved", 1234) (scServerName loadedCfg, scPort loadedCfg)
  writeFile cfgFile "{\"Port\": 4321}"
  partialCfg <- loadServerConfig cfgFile
  check "loadServerConfig applies a partial file over the defaults"
    (4321, "functional-media-server") (scPort partialCfg, scServerName partialCfg)
  removeFile cfgFile

  -- Admin module (milestone 11a, docs/todo.org section 11): the activity
  -- ring buffer, the task-due predicate, and the JSON roundtrips.
  aNow <- getCurrentTime
  let mkEntry i = ActivityEntry i aNow "Test" ("msg " <> T.pack (show i))
      (n1, ab1) = pushActivity 1 [] (mkEntry 0)
  check "pushActivity assigns the id and bumps the counter"
    (2, [(mkEntry 0) { aeId = 1 }]) (n1, ab1)
  let (nBig, bBig) = foldl (\(n, b) _ -> pushActivity n b (mkEntry 0)) (1, [])
                     [1 .. 501 :: Int]
  check "pushActivity caps the ring buffer at activityLogCap"
    activityLogCap (length bBig)
  check "pushActivity drops the oldest entry first"
    (Just 2) (aeId <$> listToMaybe bBig)
  check "pushActivity keeps the newest entry last"
    (Just 501) (aeId <$> listToMaybe (reverse bBig))
  check "pushActivity counter after 501 pushes"
    502 nBig
  let tEnabled  = ScheduledTask "t" "T" 300 True Nothing Nothing
      tDisabled = tEnabled { stEnabled = False }
      tRecent   = tEnabled { stLastRun = Just (addUTCTime (-100) aNow) }
      tOld      = tEnabled { stLastRun = Just (addUTCTime (-400) aNow) }
      tBoundary = tEnabled { stLastRun = Just (addUTCTime (-300) aNow) }
  check "taskDue: enabled and never run -> True"  True  (taskDue aNow tEnabled)
  check "taskDue: disabled -> False"               False (taskDue aNow tDisabled)
  check "taskDue: last run older than interval -> True" True (taskDue aNow tOld)
  check "taskDue: last run within interval -> False"    False (taskDue aNow tRecent)
  check "taskDue: exactly at the interval boundary -> True" True (taskDue aNow tBoundary)
  let ae = ActivityEntry 7 aNow "TaskRun" "library-rescan: ok: done"
  check "ActivityEntry JSON roundtrip"
    (Just ae) (Aeson.decode (Aeson.encode ae) :: Maybe ActivityEntry)
  check "ActivityEntry JSON uses Id/Time/Type/Message keys"
    True (all (`BSC.isInfixOf` LBS.toStrict (Aeson.encode ae))
      ["\"Id\":", "\"Time\":", "\"Type\":", "\"Message\":"])
  let stk = ScheduledTask "library-rescan" "Library rescan" 300 True (Just aNow) (Just "ok")
  check "ScheduledTask JSON roundtrip"
    (Just stk) (Aeson.decode (Aeson.encode stk) :: Maybe ScheduledTask)
  check "ScheduledTask JSON uses the PascalCase keys"
    True (all (`BSC.isInfixOf` LBS.toStrict (Aeson.encode stk))
      ["\"Id\":", "\"Name\":", "\"IntervalSeconds\":", "\"Enabled\":", "\"LastRun\":", "\"LastResult\":"])

  -- Web client (milestone 13/16, docs/todo.org section 13): the embedded
  -- single-page HTML must contain the player, the search query, and the
  -- playback/thumb/HLS/metadata links, and must be ASCII-only.
  check "web page contains a <video> player" True ("<video" `BSC.isInfixOf` LBS.toStrict webPageHtml)
  check "web page queries /items?search=" True ("/items?search=" `BSC.isInfixOf` LBS.toStrict webPageHtml)
  check "web page plays via /stream/" True ("/stream/" `BSC.isInfixOf` LBS.toStrict webPageHtml)
  check "web page links thumbs, HLS and metadata"
    True (all (`BSC.isInfixOf` LBS.toStrict webPageHtml)
      ["/items/'+it.id+'/thumb", "/videos/'+it.id+'/main.m3u8", "/items/'+it.id+'/metadata"])
  check "web page is pure ASCII" True (LBS.all (<= 0x7F) webPageHtml)

  -- Media folders (milestone 2c, docs/todo.org section 2): folder JSON,
  -- folders.json persistence, nextFolderId, the /items?libraryId= filter,
  -- and the SafeCopy v0->v1 migration of MediaItem (docs/acid-state.org).
  let mf  = MediaFolder "2" "Movies" "/tmp/movies" "movies"
      mf1 = MediaFolder "1" "Media" "/media" "mixed"
      mfMap = HM.fromList [("1", mf1), ("2", mf)]
      foldersFile = "/tmp/fms-test-folders.json"
  check "MediaFolder JSON roundtrip"
    (Just mf) (Aeson.decode (Aeson.encode mf) :: Maybe MediaFolder)
  check "MediaFolder JSON uses Id/Name/Path/Type keys"
    True (all (`BSC.isInfixOf` LBS.toStrict (Aeson.encode mf))
      ["\"Id\":", "\"Name\":", "\"Path\":", "\"Type\":"])
  saveFolders foldersFile mfMap
  loadedFolders <- loadFolders foldersFile
  check "folders save/load roundtrip" mfMap loadedFolders
  check "loadFolders of a missing file is empty"
    HM.empty =<< loadFolders "/tmp/fms-test-folders-gone.json"
  removeFile foldersFile
  check "nextFolderId after numeric ids 1,2" "3" (nextFolderId mfMap)
  check "nextFolderId of an empty map is 1" "1" (nextFolderId HM.empty)
  let qi fid title = MediaItem
        { itemId = fid, itemTitle = title, itemPath = "/" ++ T.unpack title ++ ".mp4"
        , itemSize = 1, itemMime = "video/mp4", itemAddedAt = aNow, itemFolder = fid
        , itemType = "Movie", itemSeries = Nothing, itemSeason = Nothing, itemEpisode = Nothing
        , itemExtraType = Nothing }
      qItems = [qi "1" "b", qi "2" "m", qi "1" "a"]
      qSorted = [qi "1" "a", qi "1" "b", qi "2" "m"]
  check "applyItemQuery libraryId=2 keeps only folder-2 items"
    (1, [qi "2" "m"]) (applyItemQuery [("libraryId", "2")] qItems)
  check "applyItemQuery libraryId=1 keeps only folder-1 items"
    (2, [qi "1" "a", qi "1" "b"]) (applyItemQuery [("libraryId", "1")] qItems)
  check "applyItemQuery without libraryId keeps all (sorted by title)"
    (3, qSorted) (applyItemQuery [] qItems)
  check "applyItemQuery libraryId + search combine"
    (1, [qi "1" "a"]) (applyItemQuery [("libraryId", "1"), ("search", "a")] qItems)
  check "filterByFolder Nothing is the identity" qItems (filterByFolder Nothing qItems)
  check "filterByFolder Just filters by itemFolder"
    [qi "2" "m"] (filterByFolder (Just "2") qItems)
  let v0 = MediaItemV0
        { v0ItemId = "9", v0ItemTitle = "old", v0ItemPath = "/old.mp4"
        , v0ItemSize = 10, v0ItemMime = "video/mp4", v0ItemAddedAt = aNow }
      v1 = migrateMediaItem v0
  check "migrateMediaItem v0->v1 defaults itemFolder to \"1\"" "1" (v1ItemFolder v1)
  check "migrateMediaItem keeps the other fields"
    ("9", "/old.mp4", 10) (v1ItemId v1, v1ItemPath v1, v1ItemSize v1)
  check "MediaItemV1 SafeCopy v1 serialize/deserialize roundtrip"
    (Right v1) (runGet safeGet (runPut (safePut v1)))
  check "SafeCopy reads v0 bytes as v3 via the chain v0->v1->v2->v3 (folder \"1\")"
    (Right "1") (itemFolder <$> (runGet safeGet (runPut (safePut v0))
                                 :: Either String MediaItem))
  let v2 = migrateMediaItemV1 v1
  check "migrateMediaItemV1 v1->v2 derives type Movie from video mime"
    "Movie" (v2ItemType v2)
  check "migrateMediaItemV1 v1->v2 defaults series/season/episode to Nothing"
    (Nothing, Nothing, Nothing) (v2ItemSeries v2, v2ItemSeason v2, v2ItemEpisode v2)
  check "migrateMediaItemV1 keeps id/folder"
    ("9", "1") (v2ItemId v2, v2ItemFolder v2)
  check "MediaItemV2 SafeCopy v2 serialize/deserialize roundtrip"
    (Right v2) (runGet safeGet (runPut (safePut v2)))
  check "itemTypeFromMime audio -> Audio" "Audio" (itemTypeFromMime "audio/mpeg")
  check "itemTypeFromMime other -> Unknown" "Unknown" (itemTypeFromMime "application/octet-stream")
  check "SafeCopy reads v1 bytes as v3 (type from mime)"
    (Right "Movie") (itemType <$> (runGet safeGet (runPut (safePut v1))
                                   :: Either String MediaItem))
  check "SafeCopy reads v2 bytes as v3 (extraType defaults to Nothing)"
    (Right Nothing) (itemExtraType <$> (runGet safeGet (runPut (safePut v2))
                                         :: Either String MediaItem))
  let v3 = migrateMediaItemV2 v2
  check "migrateMediaItemV2 v2->v3 defaults itemExtraType to Nothing"
    Nothing (itemExtraType v3)
  check "migrateMediaItemV2 keeps id/type/folder"
    ("9", "Movie", "1") (itemId v3, itemType v3, itemFolder v3)

  -- Item types + TV shows (milestone 4d, docs/todo.org sections 2-4).
  check "parseEpisodeFilename S01E02" (Just (1, 2)) (parseEpisodeFilename "S01E02")
  check "parseEpisodeFilename s1e2 lowercase" (Just (1, 2)) (parseEpisodeFilename "s1e2")
  check "parseEpisodeFilename S01E02E03 takes the first pair"
    (Just (1, 2)) (parseEpisodeFilename "S01E02E03")
  check "parseEpisodeFilename S10E10" (Just (10, 10)) (parseEpisodeFilename "S10E10")
  check "parseEpisodeFilename no match" Nothing (parseEpisodeFilename "movie")
  check "parseEpisodeFilename inside a longer name"
    (Just (2, 5)) (parseEpisodeFilename "My Show S02E05")
  check "classifyItem with tvshow.nfo -> Episode of the dir name"
    ("Episode", Just "MyShow", Just 1, Just 2)
    (classifyItem True "/lib/MyShow/S01E02.mp4" "S01E02")
  check "classifyItem without nfo -> Movie from mime"
    ("Movie", Nothing, Nothing, Nothing)
    (classifyItem False "/lib/movie.mp4" "movie")
  check "classifyItem without nfo, audio -> Audio"
    ("Audio", Nothing, Nothing, Nothing)
    (classifyItem False "/lib/song.mp3" "song")
  let tvNfo = BSC.pack (unlines
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        , "<tvshow>"
        , "  <title>My Show</title>"
        , "  <overview>A test show</overview>"
        , "  <genre>Drama</genre>"
        , "  <premiered>2022-01-01</premiered>"
        , "  <rating>8.0</rating>"
        , "  <tmdbid>4242</tmdbid>"
        , "</tvshow>" ])
  case parseTvShowNfo tvNfo of
    Left _ -> check "parseTvShowNfo parses a <tvshow> root" True False
    Right m -> do
      check "parseTvShowNfo parses a <tvshow> root" True True
      check "parseTvShowNfo title/overview/genres"
        (Just "My Show", Just "A test show", ["Drama"]) (metaTitle m, metaOverview m, metaGenres m)
      check "parseTvShowNfo premiered -> year 2022" (Just 2022) (metaYear m)
      check "parseTvShowNfo rating + tmdbid + source"
        (Just 8.0, Just "4242", "nfo") (metaRating m, HM.lookup "tmdb" (metaProviderIds m), metaSource m)
  check "parseTvShowNfo rejects a <movie> root"
    True (either (const True) (const False) (parseTvShowNfo (BSC.pack "<movie><title>x</title></movie>")))
  let epA = (qi "1" "S01E01") { itemType = "Episode", itemSeries = Just "MyShow", itemSeason = Just 1, itemEpisode = Just 1 }
      epB = (qi "1" "S01E02") { itemType = "Episode", itemSeries = Just "MyShow", itemSeason = Just 1, itemEpisode = Just 2 }
      epC = (qi "1" "S02E01") { itemType = "Episode", itemSeries = Just "MyShow", itemSeason = Just 2, itemEpisode = Just 1 }
      other = (qi "2" "Other") { itemType = "Episode", itemSeries = Just "OtherShow" }
      showItems = [epB, other, epA, epC, qi "1" "movie"]
  check "showsFromItems aggregates and sorts by name"
    [("MyShow", 3), ("OtherShow", 1)] (showsFromItems showItems)
  check "showsFromItems ignores non-episodes" [] (showsFromItems [qi "1" "movie"])
  check "sortEpisodes orders by season/episode"
    [epA, epB, epC] (sortEpisodes [epC, epB, epA])
  check "filterByType Episode keeps episodes in list order"
    [epB, other, epA, epC] (filterByType (Just "Episode") showItems)
  check "filterByType Movie" [qi "1" "movie"] (filterByType (Just "Movie") showItems)
  check "filterBySeries MyShow" [epB, epA, epC] (filterBySeries (Just "MyShow") showItems)
  check "applyItemQuery type=Episode (then title sort)"
    (4, [other, epA, epB, epC])
    (applyItemQuery [("type", "Episode")] showItems)
  check "applyItemQuery series=MyShow&type=Episode"
    (3, [epA, epB, epC])
    (applyItemQuery [("series", "MyShow"), ("type", "Episode")] showItems)

  -- Richer /items query (milestone 4e, docs/todo.org section 4).
  check "itemYear is Nothing (MediaItem has no year field)"
    Nothing (itemYear (qi "1" "movie"))
  check "filterByIncludeTypes case-insensitive episode"
    [epB, other, epA, epC] (filterByIncludeTypes (Just "episode") showItems)
  check "filterByIncludeTypes Movie,Episode keeps all five"
    showItems (filterByIncludeTypes (Just "Movie,Episode") showItems)
  check "filterByIncludeTypes Audio matches nothing"
    [] (filterByIncludeTypes (Just "Audio") showItems)
  check "filterByIncludeTypes empty string is identity"
    showItems (filterByIncludeTypes (Just "") showItems)
  check "filterByIncludeTypes Nothing is identity"
    showItems (filterByIncludeTypes Nothing showItems)
  check "filterByYears with a listed year matches nothing (no year field)"
    [] (filterByYears (Just "2022") showItems)
  check "filterByYears with only invalid tokens is identity"
    showItems (filterByYears (Just "abc") showItems)
  check "filterByYears Nothing is identity"
    showItems (filterByYears Nothing showItems)
  check "applyItemQuery searchTerm is an alias of search"
    (applyItemQuery [("search", "S01")] showItems)
    (applyItemQuery [("searchTerm", "S01")] showItems)
  check "applyItemQuery search wins over searchTerm when both present"
    (applyItemQuery [("search", "movie")] showItems)
    (applyItemQuery [("search", "movie"), ("searchTerm", "S01")] showItems)
  check "applyItemQuery searchTerm=S01 finds S01E01+S01E02 sorted"
    (2, [epA, epB]) (applyItemQuery [("searchTerm", "S01")] showItems)
  check "applyItemQuery includeItemTypes=Episode (then title sort)"
    (4, [other, epA, epB, epC])
    (applyItemQuery [("includeItemTypes", "Episode")] showItems)
  check "applyItemQuery includeItemTypes + years (no year field -> 0)"
    (0, []) (applyItemQuery [("includeItemTypes", "Movie,Episode"), ("years", "2022")] showItems)
  check "applyItemQuery recursive + filters accepted and ignored"
    (applyItemQuery [] showItems)
    (applyItemQuery [("recursive", "true"), ("filters", "IsPlayed,IsFavorite,IsResumable")] showItems)
  check "applyItemQuery type=Episode still works alongside 4e params"
    (4, [other, epA, epB, epC])
    (applyItemQuery [("type", "Episode"), ("recursive", "false")] showItems)

  -- User-scoped filters + fields projection (milestone 5e).
  let ud5eRec played fav pos = UserData played fav pos 0 Nothing
      ud5eMap = HM.fromList
        [ ("1", ud5eRec True True (Just 30))    -- played + favorite + resumable-but-played
        , ("2", ud5eRec False False (Just 5))   -- resumable
        , ("3", ud5eRec True False Nothing)     -- played, not resumable
        ]
      ud5eLookup = flip HM.lookup ud5eMap
      uItems = [qi "1" "a", qi "2" "b", qi "3" "c", qi "4" "d"]
  check "applyUserDataFilters IsPlayed" [qi "1" "a", qi "3" "c"]
    (applyUserDataFilters ["IsPlayed"] ud5eLookup uItems)
  check "applyUserDataFilters Unplayed (missing record = unplayed)"
    [qi "2" "b", qi "4" "d"] (applyUserDataFilters ["Unplayed"] ud5eLookup uItems)
  check "applyUserDataFilters IsFavorite" [qi "1" "a"]
    (applyUserDataFilters ["IsFavorite"] ud5eLookup uItems)
  check "applyUserDataFilters IsLiked is an alias of IsFavorite" [qi "1" "a"]
    (applyUserDataFilters ["IsLiked"] ud5eLookup uItems)
  check "applyUserDataFilters IsResumable (pos>0 and not played)" [qi "2" "b"]
    (applyUserDataFilters ["IsResumable"] ud5eLookup uItems)
  check "applyUserDataFilters tokens are case-insensitive" [qi "1" "a", qi "3" "c"]
    (applyUserDataFilters ["isplayed"] ud5eLookup uItems)
  check "applyUserDataFilters tokens are ANDed" [qi "1" "a"]
    (applyUserDataFilters ["IsPlayed", "IsFavorite"] ud5eLookup uItems)
  check "applyUserDataFilters unknown token ignored" uItems
    (applyUserDataFilters ["IsSomething"] ud5eLookup uItems)
  check "applyUserDataFilters empty list is identity" uItems
    (applyUserDataFilters [] ud5eLookup uItems)
  let iv = Aeson.toJSON (qi "1" "movie")
      lookupObj v = case v of
        Aeson.Object o -> KeyMap.toList o
        _              -> []
      keysOf v = map (Key.toText . fst) (lookupObj v)
  check "projectFields keeps requested keys (case-insensitive) plus id"
    ["id", "title"] (keysOf (projectFields ["Title"] iv))
  check "projectFields empty list is identity" iv (projectFields [] iv)
  check "projectFields keeps only id when nothing requested matches"
    ["id"] (keysOf (projectFields ["nope"] iv))
  check "projectFields non-object passthrough"
    (Aeson.String "hello") (projectFields ["id"] (Aeson.String "hello"))
  check "projectFields result still encodes"
    True (not (null (LBSC.unpack (Aeson.encode (projectFields ["type"] iv)))))
  let attached = attachUserData (Just ud5eLookup) (qi "1" "movie") iv
  check "attachUserData adds a UserData key with the looked-up record"
    (Just (Aeson.toJSON (ud5eRec True True (Just 30))))
    (case attached of
       Aeson.Object o -> KeyMap.lookup (Key.fromText "UserData") o
       _              -> Nothing)
  check "attachUserData defaults to emptyUserData for unknown items"
    (Just (Aeson.toJSON emptyUserData))
    (case attachUserData (Just ud5eLookup) (qi "9" "zz") iv of
       Aeson.Object o -> KeyMap.lookup (Key.fromText "UserData") o
       _              -> Nothing)
  check "attachUserData without a lookup is identity"
    iv (attachUserData Nothing (qi "1" "movie") iv)
  check "attachUserData + projectFields: fields=UserData keeps the record"
    [ "UserData", "id" ]
    (keysOf (projectFields ["userdata"] (attachUserData (Just ud5eLookup) (qi "1" "movie") iv)))

  -- Embedded subtitle streams (milestone 3f, docs/todo.org section 7).
  let mkStream i t c l =
        MediaStream i t c l Nothing Nothing Nothing Nothing Nothing
      probeStreams =
        [ mkStream 0 "Video" "h264" Nothing
        , mkStream 1 "Audio" "aac" (Just "eng")
        , mkStream 2 "Subtitle" "subrip" (Just "eng")
        , mkStream 3 "Subtitle" "ass" Nothing
        ]
  check "embeddedSubtitleSources keeps only Subtitle streams (index, codec, language)"
    [(2, "subrip", Just "eng"), (3, "ass", Nothing)]
    (embeddedSubtitleSources probeStreams)
  check "embeddedSubtitleSources of empty input is empty"
    [] (embeddedSubtitleSources [])
  check "embeddedSubtitleSources ignores video/audio streams"
    [] (embeddedSubtitleSources [mkStream 0 "Video" "h264" Nothing, mkStream 1 "Audio" "aac" Nothing])
  let mixedSrcs =
        [ External "/m/clip.srt" "srt"
        , External "/m/clip.vtt" "vtt"
        , Embedded 2 "subrip" (Just "eng")
        ]
  check "indexSources assigns 0-based indexes, externals first then embedded"
    [ (0, External "/m/clip.srt" "srt")
    , (1, External "/m/clip.vtt" "vtt")
    , (2, Embedded 2 "subrip" (Just "eng"))
    ]
    (indexSources mixedSrcs)
  check "embeddedToVttArgs maps stream 0:n and converts to webvtt on stdout"
    ["-v", "error", "-i", "/m/clip.mkv", "-map", "0:2", "-c:s", "webvtt", "-f", "webvtt", "pipe:1"]
    (embeddedToVttArgs "/m/clip.mkv" 2)
  -- IO: the combined source list on a real directory. A dummy media file
  -- makes ffprobe fail -> only the external entry, no crash.
  let subDir = "/tmp/fms-test-subs-3f"
  resetDir subDir
  createDirectoryIfMissing True subDir
  let dummyMedia = subDir </> "clip.mp4"
  BS.writeFile dummyMedia (BSC.pack "not a real media file")
  BS.writeFile (subDir </> "clip.srt") (BSC.pack "1\n00:00:01,000 --> 00:00:02,000\nhello\n")
  BS.writeFile (subDir </> "other.vtt") (BSC.pack "WEBVTT\n\n00:00:01.000 --> 00:00:02.000\nhi\n")
  srcs3f <- subtitleListSources dummyMedia
  check "subtitleListSources: external files first, probe failure adds no embedded"
    [ External (subDir </> "clip.srt") "srt" ]
    srcs3f
  subs3f <- subtitleList dummyMedia
  check "subtitleList keeps the milestone-3d shape (index, path, format)"
    [(0, subDir </> "clip.srt", "srt")] subs3f
  -- IO, ffprobe-guarded: a real container with an embedded srt stream.
  mFFprobe <- findExecutable "ffprobe"
  mFFmpeg3f <- findExecutable "ffmpeg"
  case (mFFprobe, mFFmpeg3f) of
    (Just _, Just _) -> do
      let mkv = subDir </> "withsubs.mkv"
          srt = subDir </> "withsubs.srt"
      BS.writeFile srt (BSC.pack "1\n00:00:01,000 --> 00:00:02,000\nembedded hello\n")
      (ecMux, _, muxErr) <- runProcessBS "ffmpeg"
        [ "-v", "error", "-f", "lavfi", "-i", "testsrc=duration=1:size=64x48:rate=5"
        , "-i", srt, "-map", "0:v", "-map", "1:s", "-c:v", "libx264"
        , "-c:s", "srt", "-y", mkv ]
      case ecMux of
        ExitFailure _ ->
          putStrLn ("SKIP: ffmpeg mux failed: " ++ show (showBS (LBS.toStrict muxErr)))
        ExitSuccess -> do
          srcsMkv <- subtitleListSources mkv
          let embOk = case srcsMkv of
                [ External p "srt", Embedded si codec _ ] ->
                  p == subDir </> "withsubs.srt" && si == 1 && codec == "subrip"
                _ -> False
          check "subtitleListSources: external withsubs.srt first, then embedded stream 1 (subrip)"
            True embOk
          subsMkv <- subtitleList mkv
          check "subtitleList on the muxed file still lists only the external file"
            [(0, subDir </> "withsubs.srt", "srt")] subsMkv
    _ -> putStrLn "SKIP: ffprobe/ffmpeg not found on PATH -- skipping embedded-subtitle IO test"

  -- Extras / special features (milestone 4g, docs/todo.org sections 2-4).
  check "extraTypeFor trailer" (Just "Trailer")
    (extraTypeFor "/m/movie-trailer.mp4" "movie-trailer")
  check "extraTypeFor case-insensitive TRAILER" (Just "Trailer")
    (extraTypeFor "/m/MOVIE-TRAILER.mp4" "MOVIE-TRAILER")
  check "extraTypeFor behind the scenes" (Just "BehindTheScenes")
    (extraTypeFor "/m/movie behind the scenes.mp4" "movie behind the scenes")
  check "extraTypeFor bts abbreviation" (Just "BehindTheScenes")
    (extraTypeFor "/m/bts.mp4" "bts")
  check "extraTypeFor deletedscene" (Just "DeletedScene")
    (extraTypeFor "/m/deletedscene.mp4" "deletedscene")
  check "extraTypeFor featurette" (Just "Featurette")
    (extraTypeFor "/m/featurette.mp4" "featurette")
  check "extraTypeFor short (substring)" (Just "Short")
    (extraTypeFor "/m/my short film.mp4" "my short film")
  check "extraTypeFor theme" (Just "ThemeSong")
    (extraTypeFor "/m/theme song.mp4" "theme song")
  check "extraTypeFor extras dir -> Other" (Just "Other")
    (extraTypeFor "/m/extras/clip.mp4" "clip")
  check "extraTypeFor specials dir -> Other" (Just "Other")
    (extraTypeFor "/m/specials/clip.mp4" "clip")
  check "extraTypeFor token in parent dir name (Trailers)" (Just "Trailer")
    (extraTypeFor "/m/Trailers/clip.mp4" "clip")
  check "extraTypeFor filename token wins over extras dir" (Just "Trailer")
    (extraTypeFor "/m/extras/movie-trailer.mp4" "movie-trailer")
  check "extraTypeFor no match -> Nothing" Nothing
    (extraTypeFor "/m/movie.mp4" "movie")
  let mainItem = (qi "1" "movie") { itemPath = "/m/movie.mp4" }
      exSameDir = (qi "2" "aaa trailer") { itemPath = "/m/movie-trailer.mp4"
                                          , itemType = "Extra", itemExtraType = Just "Trailer" }
      exOneBelow = (qi "3" "behind") { itemPath = "/m/extras/behind.mp4"
                                     , itemType = "Extra", itemExtraType = Just "BehindTheScenes" }
      exDeep = (qi "4" "deep") { itemPath = "/m/extras/deep/x.mp4"
                               , itemType = "Extra", itemExtraType = Just "Other" }
      exFar = (qi "5" "far") { itemPath = "/other/far.mp4"
                             , itemType = "Extra", itemExtraType = Just "Other" }
      plainSameDir = (qi "6" "clip") { itemPath = "/m/clip.mp4" }
      sfItems = [mainItem, exSameDir, exOneBelow, exDeep, exFar, plainSameDir]
  check "specialFeaturesOf: same dir + one level below, Extras only, sorted by title"
    [exSameDir, exOneBelow] (specialFeaturesOf mainItem sfItems)
  check "specialFeaturesOf from another dir finds only extras there (itself)"
    [exFar] (specialFeaturesOf exFar sfItems)

  -- Studios / persons / tags (milestone 4h, docs/todo.org section 3).
  let sm studios persons tags =
        sparseMeta { metaStudios = studios, metaPersons = persons, metaTags = tags }
  check "studiosFromMetadata groups/counts/sorts"
    [("StudioA",2),("StudioB",1)]
    (studiosFromMetadata [sm ["StudioB","StudioA"] [] [], sm ["StudioA"] [] [], sm [] [] []])
  check "personsFromMetadata groups/counts/sorts"
    [("Alice",2),("Bob",1)]
    (personsFromMetadata [sm [] ["Bob","Alice"] [], sm [] ["Alice"] [], sm [] [] []])
  check "tagsFromMetadata groups/counts/sorts"
    [("tag1",1),("tag2",2)]
    (tagsFromMetadata [sm [] [] ["tag2"], sm [] [] ["tag2","tag1"]])
  check "studiosFromMetadata on empty input is []" [] (studiosFromMetadata [])
  let nfo4h = BSC.pack (unlines
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        , "<movie>"
        , "  <title>T</title>"
        , "  <studio>StudioA</studio>"
        , "  <studio>StudioB</studio>"
        , "  <actor><name>Alice</name><role>R</role></actor>"
        , "  <actor><name>Bob</name></actor>"
        , "  <director>Dir Person</director>"
        , "  <tag>tag1</tag>"
        , "</movie>" ])
  case parseMovieNfo nfo4h of
    Left _ -> check "parseMovieNfo parses studio/actor/director/tag fields" True False
    Right m -> do
      check "parseMovieNfo <studio> list" ["StudioA","StudioB"] (metaStudios m)
      check "parseMovieNfo persons = actor names + director"
        ["Alice","Bob","Dir Person"] (metaPersons m)
      check "parseMovieNfo <tag> list" ["tag1"] (metaTags m)
  check "MediaMetadata JSON roundtrip keeps the new fields"
    (Just (sm ["S"] ["P"] ["t"]))
    (Aeson.decode (Aeson.encode (sm ["S"] ["P"] ["t"])) :: Maybe MediaMetadata)
  check "MediaMetadata FromJSON of pre-4h JSON (no new keys) defaults to []"
    (Just ([], [], []))
    ((\m -> (metaStudios m, metaPersons m, metaTags m))
       <$> (Aeson.decode (LBSC.pack "{\"title\":\"x\",\"genres\":[\"Drama\"],\"source\":\"nfo\"}")
            :: Maybe MediaMetadata))
  let metaOf4h studios genres persons =
        sparseMeta { metaStudios = studios, metaGenres = genres, metaPersons = persons }
      mMap4h = HM.fromList
        [ ("1", metaOf4h ["StudioA"] ["Action"] ["Alice"])
        , ("2", metaOf4h ["StudioB"] ["Drama"] ["Bob"])
        ]
      mLookup4h = flip HM.lookup mMap4h
      fItems = [qi "1" "a", qi "2" "b", qi "3" "c"]   -- item 3 has no metadata
  check "applyMetadataFilters genres=ACTION (case-insensitive), no-metadata item excluded"
    [qi "1" "a"] (applyMetadataFilters [("genres", "ACTION")] mLookup4h fItems)
  check "applyMetadataFilters genres=action,drama (ANY token intersects)"
    [qi "1" "a", qi "2" "b"] (applyMetadataFilters [("genres", "action,drama")] mLookup4h fItems)
  check "applyMetadataFilters studios=studiob" [qi "2" "b"]
    (applyMetadataFilters [("studios", "studiob")] mLookup4h fItems)
  check "applyMetadataFilters persons=Bob" [qi "2" "b"]
    (applyMetadataFilters [("persons", "Bob")] mLookup4h fItems)
  check "applyMetadataFilters different params are ANDed"
    [] (applyMetadataFilters [("genres", "action"), ("studios", "StudioB")] mLookup4h fItems)
  check "applyMetadataFilters empty params are the identity"
    fItems (applyMetadataFilters [] mLookup4h fItems)
  check "applyMetadataFilters blank param value is the identity"
    fItems (applyMetadataFilters [("genres", " , ")] mLookup4h fItems)
  check "applyMetadataFilters unknown param is ignored"
    fItems (applyMetadataFilters [("year", "2020")] mLookup4h fItems)

  putStrLn "All tests passed."
