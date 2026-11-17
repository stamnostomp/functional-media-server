module Main (main) where

import Control.Concurrent.QSemN (newQSemN, signalQSemN)
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
import Data.Word (Word8)
import qualified Data.Text as T
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), addUTCTime, getCurrentTime)
import Data.Time.LocalTime (TimeOfDay (..), timeOfDayToTime)
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

import FMS.Core.Cli (parseCliArgs)
import FMS.Core.Module (Segment (..), get, post)
import FMS.Core.Types.Config (Config (..), validateConfig)
import FMS.Core.Process (ffmpegTimeoutArgs, runProcessBS, showBS, tryAcquireSlot)
import FMS.Core.Query (applyItemQuery, applyMetadataFilters, applyUserDataFilters, attachImageBlurHashes, attachImageTags, attachTags, attachUserData, backdropImageTagsValue, imageBlurHashesValue, filterByFolder, imageTagsValue, filterByParent, filterByIncludeTypes, filterBySeries, filterByType, filterByYears, isUserDataSortKey, itemYear, latestItems, nextUpEpisodes, normalizeQueryParams, paginate, projectFields, queryParams, notInAnyCollection, searchItems, searchItemsMeta, showsFromItems, similarItems, sortByUserDataField, sortEpisodes, sortItems, specialFeaturesOf, typeFilterIncludesBoxSet, versionParam, versionPathFor, viewsFromFolders)
import FMS.Core.Blurhash (base83Alphabet, base83Decode, base83Encode, decodeBlurhashSize, encodeBlurhash)
import FMS.Core.Range (ByteRange(..), parseRangeHeader)
import FMS.Core.SceneDetect (blackdetectArgs, intervalsOverlap, mergeIntervals, parseBlackdetect, parseSilencedetect, proposeSegments, silencedetectArgs)
import FMS.Core.RateLimit (Bucket (..), newBucket, refill, requestIdHex, tryTake)
import FMS.Core.Router (allowedOriginsHeader, isOpenPath, matchSegments, normalizeSegments, parseCorsOrigins, parseTokenHeader, securityHeaders)
import FMS.Core.Types.MediaFolder (MediaFolder (..))
import FMS.Core.Types.Log (LogEntry (..), logLevelAllows, logRank, pushLogEntry)
import FMS.Core.Types.MediaItem (MediaItem (..), MediaItemV0 (..), MediaItemV1 (..), MediaItemV2 (..), MediaItemV3 (..), itemTypeFromMime, migrateMediaItem, migrateMediaItemV1, migrateMediaItemV2, migrateMediaItemV3)
import FMS.Core.Types.Policy (AccessSchedule (..), UserPolicy (..), emptyUserPolicy, itemAllowedByPolicy, ratingAllowed, ratingRank, scheduleAllowsNow, tagsAllowed)
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
import FMS.Core.Types.User (User(..), checkPasswordPolicy, toInternalJSON)
import FMS.Modules.Auth.Monad (canAccessFolder, hashPassword, verifyPassword)
import FMS.Modules.Auth.Routes (PermissionsRequest (..), PolicyPatch (..), applyPolicyPatch)
import FMS.Modules.Auth.State (loadUsers, saveUsers)
import FMS.Modules.Library.Monad (scanProgressData, withScanLock)
import FMS.Modules.Library.Watch (eventsPending, watchMediaFile)
import FMS.Modules.Library.Folders (loadFolders, nextFolderId, saveFolders, validateFolderPath)
import FMS.Modules.Library.Scan
  ( classifyItem
  , diffScan
  , extraTypeFor
  , isCircular
  , isMediaFile
  , parseEpisodeFilename
  , itemFromPath
  , mimeFor
  , groupVersions
  , normalizeVersionTitle
  , planRescan
  , scanDirectory
  , versionRank
  , walk
  , walkWithLoops
  )
import FMS.Modules.Stats.Monad (applyPlay, applyProgress, continueWatching, popular)
import FMS.Modules.Stats.Persistence (loadStats, saveStats)
import FMS.Modules.UserData.Monad (markPlayed, setFavorite, setPosition)
import FMS.Modules.UserData.Persistence (loadUserData, saveUserData)
import FMS.Modules.UserData.Routes (parseIdsBody)
import FMS.Modules.Webhooks.Persistence (loadWebhooks, saveWebhooks)
import FMS.Modules.Webhooks.Types (Webhook (..), eventMatches)
import FMS.Modules.UserData.Types (UserData (..), emptyUserData)
import FMS.Modules.Hls (hlsArgs, hlsDirFor)
import FMS.Modules.Images (imageExtFor, imagePathFor, imageTag, isValidImageType, resizeArgs, selectBackdrop)
import FMS.Modules.Stream (resolveRange)
import FMS.Modules.Subtitles (SubtitleSource (..), embeddedSubtitleSources, embeddedToVttArgs, indexSources, pickSubtitles, subtitleExts, subtitleList, subtitleListSources, subtitleToVttArgs)
import FMS.Modules.Chapters (Chapter (..), chapterImageArgs, chapterImagePathFor, chapterToJson, fallbackChapters, parseChaptersJson, probeChaptersArgs, ticksFromSeconds)
import FMS.Modules.Thumbs (thumbArgs, thumbPathFor)
import FMS.Modules.Dash (dashArgs, dashArgsWith, dashDirFor, isValidDashSegName)
import FMS.Modules.Transcode (effectiveBitrateCap, hwEncoderFor, normalizeHwAccel, transcodeArgs, transcodeArgsWith)
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
import FMS.Modules.Metadata.Api.Images (tmdbImageListToDownloads, tmdbImagesUrl)
import FMS.Modules.Metadata.Api.Tmdb (parseTmdbResults, parseTmdbTvResults, tmdbSearchTvUrl, tmdbSearchUrl)
import FMS.Modules.Metadata.Api.Types (TmdbResult (..))
import FMS.Modules.Metadata.Nfo (nfoPathFor, parseMovieNfo, parseTvShowNfo)
import FMS.Modules.Metadata.Persistence (loadMetadata, saveMetadata)
import FMS.Modules.Metadata.Routes (genresFromMetadata, personsFromMetadata, studiosFromMetadata, tagsFromMetadata, tmdbResultToSearchHit, unmatchedItems)
import FMS.Modules.Metadata.Types (MediaMetadata (..), applyManualPatch, applyProviderMerge, emptyMetadata, fieldNameFor, mergeTags, removeTag)
import FMS.Modules.Playlists.Monad (addItem, removeItem)
import FMS.Modules.Playlists.Persistence (loadPlaylists, savePlaylists)
import FMS.Modules.Playlists.Types (Playlist (..))
import qualified FMS.Modules.Collections.Monad as CM
import FMS.Modules.Collections.Persistence (loadCollections, saveCollections)
import FMS.Modules.Collections.Types (Collection (..), boxSetIdFor, boxSetItemValue, isBoxSetId)
import FMS.Modules.Devices.Persistence (loadDevices, saveDevices)
import FMS.Modules.Devices.Types (Device (..), upsertDevice)
import FMS.Modules.Segments.Persistence (loadSegments, saveSegments)
import FMS.Modules.Segments.Types (MediaSegment (..), secondsToTicks, ticksToSeconds, validSegmentType, validateSegment)
import FMS.Modules.Plugins.Types (Plugin (..), builtinPlugins, overlayPlugins, registerHook, unregisterHook)
import FMS.Modules.Socket (addConn, encodeMessage, parseApiKey, parseApiKeyFromPath, removeConn)
import FMS.Modules.Playback.Probe
  ( containerFromExtension
  , decidePlayMethod
  , directPlayable
  , directStreamable
  , parseFfprobe
  , parseFormatBitRate
  , parseFormatDuration
  , probeArgs
  , probeBitrateArgs
  , probeFile
  , transcoding
  )
import FMS.Modules.Playback.Types
  ( MediaStream(..)
  , PlaybackResponse(..)
  , defaultDeviceProfile
  , markDefaultStreams
  , selectStreams
  )
import FMS.Modules.Sessions.Monad (expireSessions, upsertSession)
import FMS.Modules.Sessions.Types (PlaySession (..))
import FMS.Modules.Admin.Monad (activityLogCap, pushActivity, taskDue)
import FMS.Modules.Admin.Types (ActivityEntry (..), ScheduledTask (..), countsByType, uptimeSeconds)
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
  -- milestone 2e: photos + books are media files with proper mimes
  check "mimeFor photo extensions"
    ["image/jpeg", "image/jpeg", "image/png", "image/gif", "image/webp", "image/bmp", "image/heic", "image/tiff"]
    (map (T.unpack . mimeFor) ["a.jpg", "a.jpeg", "a.png", "a.gif", "a.webp", "a.bmp", "a.heic", "a.tiff"])
  check "mimeFor book extensions"
    [ "application/epub+zip", "application/x-mobipocket-ebook", "application/pdf"
    , "application/vnd.comicbook+zip", "application/vnd.comicbook-rar" ]
    (map (T.unpack . mimeFor) ["a.epub", "a.mobi", "a.pdf", "a.cbz", "a.cbr"])
  check "mimeFor extension match is case-insensitive (.JPG/.PDF)"
    ["image/jpeg", "application/pdf"] (map (T.unpack . mimeFor) ["/x/P.JPG", "/x/B.PDF"])
  check "isMediaFile accepts .jpg/.epub/.pdf, still rejects .txt"
    [True, True, True, False]
    (map isMediaFile ["/x/p.jpg", "/x/b.epub", "/x/b.pdf", "/x/n.txt"])
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
        , itemVersions = []
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
        , itemVersions = []
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

  -- Milestone 4n: metadata-aware search, image tags, exclusive filter.
  let meta4n = HM.fromList
        [ ("1", emptyMetadata { metaPersons = ["Keanu Reeves", "Carrie-Anne Moss"] })
        , ("2", emptyMetadata { metaPersons = ["Harrison Ford"] })
        ]
      lookup4n i = HM.lookup i meta4n
      items4n = [ (qi4n "1" "The Matrix"), (qi4n "2" "Blade Runner"), (qi4n "3" "Some Doc") ]
      qi4n i t = MediaItem
        { itemId = i, itemTitle = t, itemPath = "/" ++ T.unpack t ++ ".mp4"
        , itemSize = 1, itemMime = "video/mp4", itemAddedAt = t4n, itemFolder = "1"
        , itemType = "Movie", itemSeries = Nothing, itemSeason = Nothing, itemEpisode = Nothing
        , itemExtraType = Nothing, itemVersions = [] }
      t4n = read "2026-01-01 00:00:00 UTC" :: UTCTime
  check "searchItemsMeta matches title" ["The Matrix"]
    (map itemTitle (searchItemsMeta lookup4n "matrix" items4n))
  check "searchItemsMeta matches a person (case-insensitive)" ["Blade Runner"]
    (map itemTitle (searchItemsMeta lookup4n "HANNOVER" items4n) ++
     map itemTitle (searchItemsMeta lookup4n "harrison" items4n))
  check "searchItemsMeta no match -> []" ([] :: [MediaItem])
    (searchItemsMeta lookup4n "spielberg" items4n)
  check "searchItemsMeta item without metadata matches on title only" ["Some Doc"]
    (map itemTitle (searchItemsMeta lookup4n "doc" items4n))
  let tagPairs = [("Primary", "p1"), ("Backdrop", "b1"), ("backdrop", "b2"), ("Logo", "l1")]
      itRaw = LBS.toStrict (Aeson.encode (imageTagsValue tagPairs))
      bdRaw = LBS.toStrict (Aeson.encode (backdropImageTagsValue tagPairs))
  check "imageTagsValue object maps type -> tag"
    [True, True, True, True]
    (map (`BSC.isInfixOf` itRaw) ["\"Primary\":\"p1\"", "\"Backdrop\":\"b1\"", "\"backdrop\":\"b2\"", "\"Logo\":\"l1\""])
  check "backdropImageTagsValue lists only backdrop tags (case-insensitive)"
    "[\"b1\",\"b2\"]" (BSC.unpack bdRaw)
  let baseItem = Aeson.toJSON (qi4n "1" "The Matrix")
      withTags = attachImageTags Nothing tagPairs baseItem
      withType = attachImageTags (Just "logo") tagPairs baseItem
      noTags   = attachImageTags Nothing [] baseItem
      wtRaw = LBS.toStrict (Aeson.encode withTags)
      wtyRaw = LBS.toStrict (Aeson.encode withType)
  check "attachImageTags adds ImageTags + BackdropImageTags to the item object"
    [True, True, True, True]
    (map (`BSC.isInfixOf` wtRaw)
      [ "\"ImageTags\":{", "\"Primary\":\"p1\"", "\"BackdropImageTags\":[\"b1\",\"b2\"]", "\"Name\":\"The Matrix\"" ])
  check "attachImageTags with imageType=Logo limits ImageTags to that type"
    [True, False]
    (map (`BSC.isInfixOf` wtyRaw) [ "\"ImageTags\":{\"Logo\":\"l1\"}", "\"Primary\":\"p1\"" ])
  check "attachImageTags with no images leaves the value unchanged"
    baseItem noTags
  check "notInAnyCollection keeps only items in no collection"
    ["1", "3"] (map (T.unpack . itemId) (notInAnyCollection (== "2") items4n))
  check "notInAnyCollection with all collected -> []"
    ([] :: [MediaItem]) (notInAnyCollection (const True) items4n)

  -- Blurhash encoder (FMS.Core.Blurhash): base83 roundtrip, output
  -- shape, and attach helpers.
  check "base83 roundtrip for 0..6888 (2 chars, full range)"
    True (all (\v -> base83Decode (base83Encode v 2) == v) [0 .. 6888])
  check "base83 roundtrip for 3-char stride sample"
    True (all (\v -> base83Decode (base83Encode v 3) == v) [0, 977 .. 571786])
  check "base83 alphabet is the exact 0-9A-Za-z+symbols string"
    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz#$%*+,-.:;=?@[]^_{|}~"
    base83Alphabet
  let gray8 = replicate (8 * 8 * 3) 128 :: [Word8]
      hGray = encodeBlurhash 8 8 4 3 gray8
  check "uniform gray 8x8 4x3: hash length is 1+1+4+2*(12-1) = 28"
    28 (length hGray)
  check "uniform gray 8x8 4x3: decodeBlurhashSize is (4,3)"
    (Just (4, 3)) (decodeBlurhashSize hGray)
  check "4x3 size flag: first char is base83 of 21"
    (base83Encode 21 1) (take 1 hGray)
  check "black vs white solid images give different hashes"
    True (encodeBlurhash 8 8 4 3 (replicate 192 0)
            /= encodeBlurhash 8 8 4 3 (replicate 192 255))
  let grad w h = [ fromIntegral ((x * 37 + y * 11) `mod` 256 :: Int)
                 | y <- [0 .. h - 1], x <- [0 .. w - 1], _ <- [1 .. 3 :: Int] ] :: [Word8]
  check "4x3 vs 8x6 gradient images give different hashes"
    True (encodeBlurhash 4 3 4 3 (grad 4 3) /= encodeBlurhash 8 6 4 3 (grad 8 6))
  check "encodeBlurhash rejects bad pixel count (returns empty)"
    "" (encodeBlurhash 8 8 4 3 (replicate 10 0))
  -- 32x32 deterministic LCG image, cross-validated against the official
  -- woltapp/blurhash reference algorithm (Python transcription).
  let lcgPix :: Int -> [Word8]
      lcgPix n = go (12345 :: Int) n
        where
          go :: Int -> Int -> [Word8]
          go _ 0 = []
          go s k = let s' = (s * 1103515245 + 12345) `mod` 2147483648
                       b = fromIntegral ((s' `div` 65536) `mod` 256) :: Word8
                   in b : go s' (k - 1)
  check "32x32 LCG image 4x3 matches reference implementation vector"
    "L7HetU-M+I;B?dkl#8RE$kq}VgcC" (encodeBlurhash 32 32 4 3 (lcgPix (32 * 32 * 3)))
  let bhPairs = [("Primary", "LEHV6nWB2yk8pyo0adR*.7kCMdnj"), ("Backdrop", ""), ("Logo", "L00000fQfQfQ")]
      bhRaw = LBS.toStrict (Aeson.encode (imageBlurHashesValue bhPairs))
  check "imageBlurHashesValue skips empty hashes"
    [True, False, True]
    (map (`BSC.isInfixOf` bhRaw)
      [ "\"Primary\":\"LEHV6nWB2yk8pyo0adR*.7kCMdnj\"", "\"Backdrop\"", "\"Logo\":\"L00000fQfQfQ\"" ])
  check "attachImageBlurHashes adds ImageBlurHashes to an item object"
    [True, True]
    (map (`BSC.isInfixOf` LBS.toStrict (Aeson.encode (attachImageBlurHashes bhPairs baseItem)))
      [ "\"ImageBlurHashes\":", "\"Primary\":\"LEHV6nWB2yk8pyo0adR*.7kCMdnj\"" ])
  check "attachImageBlurHashes with all-empty hashes leaves value unchanged"
    baseItem (attachImageBlurHashes [("Primary", "")] baseItem)

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
      u1 = User "1" "admin" "$2y$14$somehash" True ["key1"] Nothing emptyUserPolicy True False
      u2 = User "2" "bob" "hash2" False [] (Just ["1"]) emptyUserPolicy True False
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
      uj3 = LBS.toStrict (Aeson.encode (User "3" "nopw" "" False [] Nothing emptyUserPolicy True False))
  check "User ToJSON omits passwordHash" False ("passwordHash" `BSC.isInfixOf` uj1)
  check "User ToJSON has hasPassword:true when a hash is set"
    True ("\"HasPassword\":true" `BSC.isInfixOf` uj1)
  check "User ToJSON has hasPassword:false when the hash is empty"
    True ("\"HasPassword\":false" `BSC.isInfixOf` uj3)
  check "User ToJSON carries id/name/isAdmin"
    True (all (`BSC.isInfixOf` uj1) ["\"Id\":\"1\"", "\"Name\":\"admin\"", "\"IsAdministrator\":true"])
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

  -- Milestone 8b (password policy): checkPasswordPolicy matrix, old
  -- users.json compat (missing mustChangePassword -> False), roundtrip,
  -- public MustChangePassword key.
  check "checkPasswordPolicy: 0 disables the policy"
    [Right (), Right ()] [checkPasswordPolicy 0 "", checkPasswordPolicy 0 "x"]
  check "checkPasswordPolicy: negative min length disables the policy"
    (Right ()) (checkPasswordPolicy (-3) "")
  check "checkPasswordPolicy: exact boundary passes"
    (Right ()) (checkPasswordPolicy 8 "12345678")
  check "checkPasswordPolicy: too short fails with the length in the message"
    (Left "password must be at least 8 characters") (checkPasswordPolicy 8 "1234567")
  check "checkPasswordPolicy: counts characters (unicode chars count once)"
    (Right ()) (checkPasswordPolicy 3 "abc")
  check "User FromJSON without mustChangePassword (pre-8b file) defaults to False"
    (Just False)
    (userMustChangePassword <$> (Aeson.decode "{\"userId\":\"1\",\"userName\":\"a\",\"passwordHash\":\"h\",\"isAdmin\":true,\"apiKeys\":[]}" :: Maybe User))
  check "User internal JSON roundtrip keeps mustChangePassword"
    (Just (User "9" "z" "h" False [] Nothing emptyUserPolicy True True))
    (Aeson.decode (Aeson.encode (toInternalJSON (User "9" "z" "h" False [] Nothing emptyUserPolicy True True))) :: Maybe User)
  check "User ToJSON exposes MustChangePassword and not mustChangePassword"
    (True, False)
    (let ej = LBS.toStrict (Aeson.encode u1)
     in ("\"MustChangePassword\":false" `BSC.isInfixOf` ej,
         "mustChangePassword" `BSC.isInfixOf` ej))
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
        , metaOfficialRating = Nothing
        , metaRuntimeMinutes = Just 95
        , metaProviderIds    = HM.fromList [("tmdb", "42"), ("imdb", "tt1234567")]
        , metaSource         = "nfo"
        , metaLockedFields   = []                         -- milestone 4k
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
        (Just "Only Title") Nothing [] [] [] [] Nothing Nothing Nothing Nothing HM.empty "nfo" []
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

  -- Milestone 4m: TV search URL + parser, RemoteSearch hit mapping, and
  -- the unmatched queue predicate.
  check "tmdbSearchTvUrl is the /search/tv endpoint with the encoded query"
    True ("https://api.themoviedb.org/3/search/tv?api_key=TESTKEY&query=a%20b"
            == tmdbSearchTvUrl "TESTKEY" "a b")
  let tvFixture = LBS.fromStrict (BSC.pack (unlines
        [ "{\"page\":1,\"results\":["
        , "  {\"id\":1399,\"name\":\"Game of Thrones\",\"overview\":\"Seven noble families fight.\",\"first_air_date\":\"2011-04-17\",\"vote_average\":8.4},"
        , "  {\"id\":1402,\"name\":\"The Wire\",\"first_air_date\":\"2002-06-02\"}"
        , "]}"
        ]))
  check "parseTmdbTvResults maps name/first_air_date onto TmdbResult"
    (Right [ TmdbResult 1399 "Game of Thrones" (Just "Seven noble families fight.") (Just 2011) (Just 8.4)
           , TmdbResult 1402 "The Wire" Nothing (Just 2002) Nothing ])
    (parseTmdbTvResults tvFixture)
  check "parseTmdbTvResults on invalid JSON is Left"
    True (isLeftStr (parseTmdbTvResults (LBS.fromStrict (BSC.pack "nope"))))
  let hit = tmdbResultToSearchHit "Movie" (TmdbResult 550 "Fight Club" (Just "An insomniac office worker.") (Just 1999) (Just 8.433))
      hitRaw = LBS.toStrict (Aeson.encode hit)
  check "tmdbResultToSearchHit emits the Jellyfin RemoteSearchResult keys (PascalCase)"
    [True, True, True, True, True, True]
    (map (`BSC.isInfixOf` hitRaw)
      [ "\"Id\":\"550\"", "\"Name\":\"Fight Club\"", "\"Type\":\"Movie\""
      , "\"ProductionYear\":1999", "\"ProviderIds\":{\"Tmdb\":550}", "\"Overview\":\"An insomniac" ])
  check "tmdbResultToSearchHit type=tv -> Series"
    True ("\"Type\":\"Series\"" `BSC.isInfixOf`
            LBS.toStrict (Aeson.encode (tmdbResultToSearchHit "Series" (TmdbResult 1399 "Game of Thrones" Nothing (Just 2011) Nothing))))
  let mkU i = MediaItem
        { itemId = i, itemTitle = "t" <> i, itemPath = "/" ++ T.unpack i ++ ".mp4"
        , itemSize = 1, itemMime = "video/mp4", itemAddedAt = t4m, itemFolder = "1"
        , itemType = "Movie", itemSeries = Nothing, itemSeason = Nothing, itemEpisode = Nothing
        , itemExtraType = Nothing, itemVersions = [] }
      t4m = read "2026-01-01 00:00:00 UTC" :: UTCTime
      uItems4m = [mkU "1", mkU "2", mkU "3"]
  check "unmatchedItems keeps only items without metadata"
    ["2", "3"] (map (T.unpack . itemId) (unmatchedItems (== "1") uItems4m))
  check "unmatchedItems with all matched -> []"
    ([] :: [MediaItem]) (unmatchedItems (const True) uItems4m)
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
  let vcodec c = MediaStream 0 "Video"   c Nothing Nothing Nothing Nothing Nothing Nothing False
      acodec c = MediaStream 1 "Audio"   c Nothing Nothing Nothing Nothing Nothing Nothing False
      sub    c = MediaStream 2 "Subtitle" c Nothing Nothing Nothing Nothing Nothing Nothing False
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

  -- Milestone 6i: device profile + audio/subtitle stream selection
  -- (docs/todo.org section 6).
  let ms6i i t c = MediaStream i t c Nothing Nothing Nothing Nothing Nothing Nothing False
      streams6i =
        [ ms6i 0 "Video" "h264"
        , ms6i 1 "Audio" "aac"
        , ms6i 2 "Audio" "ac3"
        , ms6i 3 "Subtitle" "subrip"
        , ms6i 4 "Subtitle" "ass"
        ]
      selIdx (mA, mS) = (msIndex <$> mA, msIndex <$> mS)
      profRaw = LBS.toStrict (Aeson.encode defaultDeviceProfile)
  check "defaultDeviceProfile JSON shape (Name + 3 profile arrays)"
    [True, True, True, True]
    (map (`BSC.isInfixOf` profRaw)
      [ "\"Name\":\"Default\"", "\"DirectPlayProfiles\":", "\"TranscodingProfiles\":", "\"SubtitleProfiles\":[]" ])
  check "defaultDeviceProfile video entry covers mp4/h264/aac and subtitle srt"
    [True, True, True, True]
    (map (`BSC.isInfixOf` profRaw)
      [ "\"Container\":\"mp4,", "\"VideoCodec\":\"h264,", "\"AudioCodec\":\"aac,", "\"Container\":\"srt," ])
  check "defaultDeviceProfile transcode entry is mp4/h264/aac"
    [True, True, True]
    (map (`BSC.isInfixOf` profRaw)
      [ "\"TranscodingProfiles\":", "\"Container\":\"mp4\"", "\"VideoCodec\":\"h264\"" ])
  check "selectStreams by index (audio 2, subtitle 4)"
    (Just 2, Just 4) (selIdx (selectStreams (Just 2) (Just 4) streams6i))
  check "selectStreams audio falls back to first audio when index unmatched"
    (Just 1, Nothing) (selIdx (selectStreams (Just 99) Nothing streams6i))
  check "selectStreams no request -> first audio, no subtitle"
    (Just 1, Nothing) (selIdx (selectStreams Nothing Nothing streams6i))
  check "selectStreams negative index means none (Jellyfin -1 convention)"
    (Nothing, Nothing) (selIdx (selectStreams (Just (-1)) (Just (-1)) streams6i))
  check "selectStreams subtitle has NO fallback when unmatched"
    (Just 1, Nothing) (selIdx (selectStreams Nothing (Just 99) streams6i))
  check "selectStreams empty stream list -> (Nothing, Nothing)"
    (Nothing, Nothing) (selIdx (selectStreams (Just 1) (Just 3) []))
  let (mA6i, mS6i) = selectStreams (Just 2) (Just 4) streams6i
  check "markDefaultStreams marks chosen audio+subtitle, nothing else"
    [False, False, True, False, True]
    (map msIsDefault (markDefaultStreams mA6i mS6i streams6i))
  check "markDefaultStreams with no selection marks nothing"
    [False, False, False, False, False]
    (map msIsDefault (markDefaultStreams Nothing Nothing streams6i))
  check "MediaStream JSON carries IsDefault key"
    True ("\"IsDefault\":false" `BSC.isInfixOf` LBS.toStrict (Aeson.encode (ms6i 0 "Video" "h264")))
  let pr6iRaw = LBS.toStrict (Aeson.encode
        (PlaybackResponse [] "sess" "DirectPlay" Nothing Nothing (Just 1) (Just 3)))
  check "PlaybackResponse echoes AudioStreamIndex/SubtitleStreamIndex when set"
    [True, True, True]
    (map (`BSC.isInfixOf` pr6iRaw)
      [ "\"AudioStreamIndex\":1", "\"SubtitleStreamIndex\":3", "\"PlayMethod\":\"DirectPlay\"" ])
  let pr6iRawN = LBS.toStrict (Aeson.encode
        (PlaybackResponse [] "sess" "Transcode" (Just 8000000) (Just "/stream/1/transcode?version=0") Nothing Nothing))
  check "PlaybackResponse omits stream indexes when Nothing, keeps bitrate/url"
    [False, False, True, True]
    (map (`BSC.isInfixOf` pr6iRawN)
      [ "\"AudioStreamIndex\"", "\"SubtitleStreamIndex\"", "\"MaxStreamingBitrate\":8000000", "\"TranscodingUrl\":\"/stream/1/transcode?version=0\"" ])

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
    (Just (ServerConfigPatch (Just "My Media") Nothing Nothing Nothing Nothing Nothing Nothing))
    (Aeson.decode "{\"ServerName\":\"My Media\"}" :: Maybe ServerConfigPatch)
  check "ServerConfigPatch empty object decodes to all-Nothing"
    (Just (ServerConfigPatch Nothing Nothing Nothing Nothing Nothing Nothing Nothing))
    (Aeson.decode "{}" :: Maybe ServerConfigPatch)
  let patch = fromMaybe (ServerConfigPatch Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
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

  -- Milestone 10b (startup wizard + branding): ServerConfig with the new
  -- fields, pre-10b config.json compatibility, patch set/clear semantics.
  check "defaultServerConfig 10b defaults (wizard not completed, empty branding)"
    (False, "", "")
    ( scStartupCompleted defaultServerConfig, scLoginDisclaimer defaultServerConfig
    , scCustomCss defaultServerConfig )
  check "ServerConfig FromJSON of pre-10b JSON (no new keys) -> defaults"
    (Just (False, "", ""))
    ((\sc -> (scStartupCompleted sc, scLoginDisclaimer sc, scCustomCss sc)) <$>
      (Aeson.decode "{\"ServerName\":\"Old\",\"RequireAuth\":true,\"LibraryPath\":\"/m\",\"Port\":1234}"
        :: Maybe ServerConfig))
  check "ServerConfig FromJSON of an empty object -> all defaults"
    (Just defaultServerConfig) (Aeson.decode "{}" :: Maybe ServerConfig)
  check "ServerConfig JSON roundtrip keeps the 10b keys"
    (Just True)
    (scStartupCompleted <$>
      (Aeson.decode (Aeson.encode (defaultServerConfig { scStartupCompleted = True }))
        :: Maybe ServerConfig))
  let p10b = fromMaybe (ServerConfigPatch Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
             (Aeson.decode "{\"StartupCompleted\":true,\"LoginDisclaimer\":\"hi\",\"CustomCss\":\"body{}\"}"
               :: Maybe ServerConfigPatch)
      m10b = applyServerConfigPatch defaultServerConfig p10b
  check "applyServerConfigPatch sets the 10b fields"
    (True, "hi", "body{}")
    (scStartupCompleted m10b, scLoginDisclaimer m10b, scCustomCss m10b)
  let p10bClear = fromMaybe (ServerConfigPatch Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
                  (Aeson.decode "{\"LoginDisclaimer\":\"\"}" :: Maybe ServerConfigPatch)
      m10bClear = applyServerConfigPatch m10b p10bClear
  check "applyServerConfigPatch can clear a branding field to empty"
    ("", True, "body{}")
    (scLoginDisclaimer m10bClear, scStartupCompleted m10bClear, scCustomCss m10bClear)
  check "ServerConfigPatch null decodes to Nothing (= keep current, patch convention)"
    (Just Nothing)
    (scpLoginDisclaimer <$>
      (Aeson.decode "{\"LoginDisclaimer\":null}" :: Maybe ServerConfigPatch))
  check "applyServerConfigPatch with a null field keeps the base value"
    "hi"
    (scLoginDisclaimer (applyServerConfigPatch m10b
      (fromMaybe (ServerConfigPatch Nothing Nothing Nothing Nothing Nothing Nothing Nothing)
        (Aeson.decode "{\"LoginDisclaimer\":null}" :: Maybe ServerConfigPatch))))
  check "ServerConfigPatch ToJSON roundtrip with 10b keys"
    (Just p10b) (Aeson.decode (Aeson.encode p10b) :: Maybe ServerConfigPatch)
  check "isOpenPath: wizard + branding paths are open"
    [True, True, True, False]
    (map isOpenPath [["System","Wizard","Startup"], ["System","Wizard","Completed"], ["Branding","Configuration"], ["System","Configuration"]])

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

  -- Milestone 10d (/Dashboard aggregate): pure countsByType + uptimeSeconds.
  let mkI t = MediaItem
        { itemId = "1", itemTitle = "x", itemPath = "/x.mp4"
        , itemSize = 1, itemMime = "video/mp4", itemAddedAt = aNow, itemFolder = "1"
        , itemType = t, itemSeries = Nothing, itemSeason = Nothing, itemEpisode = Nothing
        , itemExtraType = Nothing, itemVersions = [] }
      cRaw = LBS.toStrict (Aeson.encode (countsByType [mkI "Movie", mkI "Movie", mkI "Photo", mkI "Episode"]))
  check "countsByType aggregates per type"
    [True, True, True]
    (map (`BSC.isInfixOf` cRaw) ["\"Movie\":2", "\"Photo\":1", "\"Episode\":1"])
  check "countsByType empty list -> empty object"
    "{}" (BSC.unpack (LBS.toStrict (Aeson.encode (countsByType []))))
  let t10d = aNow
      t210d = addUTCTime 90 t10d
  check "uptimeSeconds rounds to whole seconds"
    [0, 90, 90] [uptimeSeconds t10d t10d, uptimeSeconds t210d t10d, uptimeSeconds (addUTCTime 90.4 t10d) t10d]
  check "uptimeSeconds never negative (clock skew)"
    0 (uptimeSeconds t10d t210d)
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
      ["/items/'+it.Id+'/thumb", "/videos/'+it.Id+'/main.m3u8", "/items/'+it.Id+'/metadata"])
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
        , itemExtraType = Nothing, itemVersions = [] }
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
  -- milestone 2e: photo/book type matrix
  check "itemTypeFromMime video/* -> Movie" ["Movie", "Movie"] (map itemTypeFromMime ["video/mp4", "video/x-matroska"])
  check "itemTypeFromMime image/* -> Photo"
    ["Photo", "Photo", "Photo"] (map itemTypeFromMime ["image/jpeg", "image/png", "image/heic"])
  check "itemTypeFromMime book mimes -> Book"
    ["Book", "Book", "Book", "Book", "Book"]
    (map itemTypeFromMime
      [ "application/pdf", "application/epub+zip", "application/x-mobipocket-ebook"
      , "application/vnd.comicbook+zip", "application/vnd.comicbook-rar" ])
  check "itemTypeFromMime unknown application/* -> Unknown"
    "Unknown" (itemTypeFromMime "application/zip")
  check "SafeCopy reads v1 bytes as v4 (type from mime)"
    (Right "Movie") (itemType <$> (runGet safeGet (runPut (safePut v1))
                                   :: Either String MediaItem))
  check "SafeCopy reads v2 bytes as v4 (extraType defaults to Nothing)"
    (Right Nothing) (itemExtraType <$> (runGet safeGet (runPut (safePut v2))
                                         :: Either String MediaItem))
  let v3 = migrateMediaItemV2 v2
  check "migrateMediaItemV2 v2->v3 defaults v3ItemExtraType to Nothing"
    Nothing (v3ItemExtraType v3)
  check "migrateMediaItemV2 keeps id/type/folder"
    ("9", "Movie", "1") (v3ItemId v3, v3ItemType v3, v3ItemFolder v3)
  check "MediaItemV3 SafeCopy v3 serialize/deserialize roundtrip"
    (Right v3) (runGet safeGet (runPut (safePut v3)))
  check "SafeCopy reads v3 bytes as v4 (versions default to [])"
    (Right []) (itemVersions <$> (runGet safeGet (runPut (safePut v3))
                                  :: Either String MediaItem))
  let v4 = migrateMediaItemV3 v3
  check "migrateMediaItemV3 v3->v4 defaults itemVersions to []"
    [] (itemVersions v4)
  check "migrateMediaItemV3 keeps id/type/folder/extraType"
    ("9", "Movie", "1", Nothing) (itemId v4, itemType v4, itemFolder v4, itemExtraType v4)
  check "MediaItem v4 SafeCopy roundtrip keeps versions"
    (Right (v4 { itemVersions = ["/m/a.1080p.mkv", "/m/a.2160p.mkv"] }))
    (runGet safeGet (runPut (safePut (v4 { itemVersions = ["/m/a.1080p.mkv", "/m/a.2160p.mkv"] })))
      :: Either String MediaItem)

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
    ["Id", "Name"] (keysOf (projectFields ["Name"] iv))
  check "projectFields empty list is identity" iv (projectFields [] iv)
  check "projectFields keeps only id when nothing requested matches"
    ["Id"] (keysOf (projectFields ["nope"] iv))
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
    [ "Id", "UserData" ]
    (keysOf (projectFields ["userdata"] (attachUserData (Just ud5eLookup) (qi "1" "movie") iv)))

  -- Embedded subtitle streams (milestone 3f, docs/todo.org section 7).
  let mkStream i t c l =
        MediaStream i t c l Nothing Nothing Nothing Nothing Nothing False
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
                                          , itemType = "Extra", itemExtraType = Just "Trailer", itemVersions = [] }
      exOneBelow = (qi "3" "behind") { itemPath = "/m/extras/behind.mp4"
                                     , itemType = "Extra", itemExtraType = Just "BehindTheScenes", itemVersions = [] }
      exDeep = (qi "4" "deep") { itemPath = "/m/extras/deep/x.mp4"
                               , itemType = "Extra", itemExtraType = Just "Other", itemVersions = [] }
      exFar = (qi "5" "far") { itemPath = "/other/far.mp4"
                             , itemType = "Extra", itemExtraType = Just "Other", itemVersions = [] }
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

  -- Collections / BoxSets (milestone 7): pure helpers, JSON, boxset
  -- pseudo-item synthesis, persistence roundtrip.
  check "collections addItem appends, dedupes, keeps order"
    ["1", "2", "3"] (CM.addItem "3" (CM.addItem "2" (CM.addItem "1" [])))
  check "collections addItem of an existing id is identity"
    ["1", "2"] (CM.addItem "1" ["1", "2"])
  check "collections removeItem removes all occurrences"
    ["2"] (CM.removeItem "1" ["1", "2", "1"])
  let col = Collection "abc" "Favs" ["1", "2"] aNow
  check "Collection JSON roundtrip" (Just col) (Aeson.decode (Aeson.encode col) :: Maybe Collection)
  check "Collection JSON keys are PascalCase like Playlist"
    True (all (`BSC.isInfixOf` LBS.toStrict (Aeson.encode col))
      [ "\"Id\":", "\"Name\":", "\"ItemIds\":", "\"AddedAt\":" ])
  check "boxSetIdFor prefixes the collection id" "boxset-abc" (boxSetIdFor "abc")
  check "isBoxSetId recognises pseudo-item ids" (True, False) (isBoxSetId "boxset-abc", isBoxSetId "1")
  let bvJson = LBSC.unpack (Aeson.encode (boxSetItemValue col))
  check "boxSetItemValue has Id/Name/Type/ItemCount keys (milestone 17b)"
    True (  "\"Id\":\"boxset-abc\"" `isInfixOf` bvJson
         && "\"Name\":\"Favs\"" `isInfixOf` bvJson
         && "\"Type\":\"BoxSet\"" `isInfixOf` bvJson
         && "\"ItemCount\":2" `isInfixOf` bvJson )
  check "typeFilterIncludesBoxSet type=BoxSet" True (typeFilterIncludesBoxSet [("type", "BoxSet")])
  check "typeFilterIncludesBoxSet is case-insensitive" True (typeFilterIncludesBoxSet [("type", "boxset")])
  check "typeFilterIncludesBoxSet includeItemTypes=Movie,BoxSet" True
    (typeFilterIncludesBoxSet [("includeItemTypes", "Movie,BoxSet")])
  check "typeFilterIncludesBoxSet type=Movie is False" False (typeFilterIncludesBoxSet [("type", "Movie")])
  check "typeFilterIncludesBoxSet with no params is False" False (typeFilterIncludesBoxSet [])
  let colFile = "/tmp/fms-test-collections.json"
      colMap = HM.fromList [("abc", col), ("def", Collection "def" "Other" [] aNow)]
  saveCollections colFile colMap
  loadedCols <- loadCollections colFile
  check "saveCollections/loadCollections roundtrip" colMap loadedCols
  removeFile colFile
  check "loadCollections of a missing file is empty"
    HM.empty =<< loadCollections "/tmp/fms-test-collections-gone.json"

  -- WebSocket socket API (milestone 8): pure message encoding, api_key
  -- extraction, and the connection-registry helpers.
  let kaMsg = LBSC.unpack (encodeMessage "KeepAlive" (Aeson.object []))
  check "encodeMessage shape {\"MessageType\",\"Data\"}"
    True (  "\"MessageType\":\"KeepAlive\"" `isInfixOf` kaMsg
         && "\"Data\":{}" `isInfixOf` kaMsg )
  let paMsg = LBSC.unpack (encodeMessage "LibraryChanged" (Aeson.toJSON (["a", "b"] :: [T.Text])))
  check "encodeMessage carries the Data payload"
    True (  "\"MessageType\":\"LibraryChanged\"" `isInfixOf` paMsg
         && "\"Data\":[\"a\",\"b\"]" `isInfixOf` paMsg )
  check "parseApiKey reads api_key" (Just "k1") (parseApiKey [("api_key", "k1")])
  check "parseApiKey falls back to token" (Just "t1") (parseApiKey [("token", "t1")])
  check "parseApiKey prefers api_key over token" (Just "k1")
    (parseApiKey [("api_key", "k1"), ("token", "t1")])
  check "parseApiKey ignores empty values" Nothing (parseApiKey [("api_key", "")])
  check "parseApiKey with no params is Nothing" Nothing (parseApiKey [])
  check "parseApiKeyFromPath parses /Socket?api_key=k9"
    (Just "k9") (parseApiKeyFromPath (BSC.pack "/Socket?api_key=k9"))
  check "parseApiKeyFromPath with token param and extra params"
    (Just "t2") (parseApiKeyFromPath (BSC.pack "/socket?foo=1&token=t2"))
  check "parseApiKeyFromPath without query is Nothing"
    Nothing (parseApiKeyFromPath (BSC.pack "/Socket"))
  check "addConn appends"
    ([(1, "c1"), (2, "c2")] :: [(Int, T.Text)])
    (addConn (2 :: Int) ("c2" :: T.Text) (addConn (1 :: Int) "c1" []))
  check "removeConn removes only the matching key"
    ([(2, "c2")] :: [(Int, T.Text)])
    (removeConn (1 :: Int) [(1, "c1"), (2, "c2")])
  check "removeConn of a missing key is identity"
    ([(1, "c1")] :: [(Int, T.Text)])
    (removeConn (9 :: Int) [(1, "c1")])

  -- Milestone 9: parental controls (ratingRank/ratingAllowed/tagsAllowed/
  -- scheduleAllowsNow/itemAllowedByPolicy) + enable/disable (User defaults,
  -- PolicyPatch merge, NFO <mpaa>).
  let pol9 = UserPolicy (Just "PG-13") ["Horror"] [AccessSchedule 0 9 17] Nothing
  check "ratingRank movie ratings" [1,2,3,4,5] (map ratingRank ["G","PG","PG-13","R","NC-17"])
  check "ratingRank tv ratings" [1,2,2,3,4,5] (map ratingRank ["TV-Y","TV-Y7","TV-G","TV-PG","TV-14","TV-MA"])
  check "ratingRank is case-insensitive and strips whitespace" 3 (ratingRank " pg-13 ")
  check "ratingRank of an unknown rating is 99 (most restrictive)" 99 (ratingRank "XXX")
  check "ratingAllowed: no max allows everything" True (ratingAllowed Nothing (Just "R"))
  check "ratingAllowed: unrated item passes" True (ratingAllowed (Just "G") Nothing)
  check "ratingAllowed: within max" True (ratingAllowed (Just "PG-13") (Just "PG"))
  check "ratingAllowed: above max is blocked" False (ratingAllowed (Just "PG-13") (Just "R"))
  check "ratingAllowed: equal to max is allowed, case-insensitive" True (ratingAllowed (Just "R") (Just "r"))
  check "tagsAllowed: no blocked tags" True (tagsAllowed [] ["a","b"])
  check "tagsAllowed: disjoint sets" True (tagsAllowed ["Horror"] ["Comedy"])
  check "tagsAllowed: intersection is blocked, case-insensitive" False (tagsAllowed ["horror"] ["Horror","comedy"])
  let sun10 = UTCTime (fromGregorian 2024 1 7) (timeOfDayToTime (TimeOfDay 10 0 0))  -- 2024-01-07 = Sunday 10:00
      sun20 = UTCTime (fromGregorian 2024 1 7) (timeOfDayToTime (TimeOfDay 20 0 0))  -- Sunday 20:00
      mon10 = UTCTime (fromGregorian 2024 1 8) (timeOfDayToTime (TimeOfDay 10 0 0))  -- Monday 10:00
  check "scheduleAllowsNow: empty schedule always allows" True (scheduleAllowsNow [] sun20)
  check "scheduleAllowsNow: inside the window" True (scheduleAllowsNow [AccessSchedule 0 9 17] sun10)
  check "scheduleAllowsNow: outside the window (hour)" False (scheduleAllowsNow [AccessSchedule 0 9 17] sun20)
  check "scheduleAllowsNow: wrong day" False (scheduleAllowsNow [AccessSchedule 0 9 17] mon10)
  check "scheduleAllowsNow: any of several windows" True
    (scheduleAllowsNow [AccessSchedule 1 0 8, AccessSchedule 0 9 17] sun10)
  check "itemAllowedByPolicy: empty policy allows anything" True (itemAllowedByPolicy emptyUserPolicy Nothing)
  check "itemAllowedByPolicy: blocked tag" False
    (itemAllowedByPolicy pol9 (Just sparseMeta { metaTags = ["Horror"] }))
  check "itemAllowedByPolicy: rating above max" False
    (itemAllowedByPolicy pol9 (Just sparseMeta { metaOfficialRating = Just "R" }))
  check "itemAllowedByPolicy: ok item passes" True
    (itemAllowedByPolicy pol9 (Just sparseMeta { metaTags = ["Comedy"], metaOfficialRating = Just "PG" }))
  check "User FromJSON of pre-9 JSON (no policy/enabled) gets defaults"
    (Just (emptyUserPolicy, True))
    ((\u -> (userPolicy u, userEnabled u)) <$>
      (Aeson.decode (LBSC.pack "{\"userId\":\"9\",\"userName\":\"old\",\"passwordHash\":\"h\",\"isAdmin\":false,\"apiKeys\":[]}")
        :: Maybe User))
  check "UserPolicy JSON roundtrip" (Just pol9) (Aeson.decode (Aeson.encode pol9) :: Maybe UserPolicy)
  check "PolicyPatch partial decode + applyPolicyPatch merge (absent fields kept)"
    (UserPolicy (Just "R") ["Horror","Scary"] [AccessSchedule 0 9 17] Nothing)
    (applyPolicyPatch (UserPolicy (Just "PG") ["Horror"] [AccessSchedule 0 9 17] Nothing)
      (either (const (PolicyPatch Nothing Nothing Nothing Nothing)) id
        (Aeson.eitherDecode (LBSC.pack "{\"MaxParentalRating\":\"R\",\"BlockedTags\":[\"Horror\",\"Scary\"]}"))))
  check "PolicyPatch explicit null clears the max rating"
    Nothing
    (policyMaxRating (applyPolicyPatch pol9 (PolicyPatch (Just Nothing) Nothing Nothing Nothing)))
  check "parseMovieNfo reads <mpaa> into officialRating"
    (Just (Just "PG-13"))
    (metaOfficialRating <$> either (const Nothing) Just
      (parseMovieNfo (BSC.pack "<movie><title>x</title><mpaa>PG-13</mpaa></movie>")))

  -- Milestone 5f (watch history): sortByUserDataField + batch ids body.
  let mi5 i t = MediaItem
        { itemId = i, itemTitle = t, itemPath = "/" ++ T.unpack t ++ ".mp4"
        , itemSize = 1, itemMime = "video/mp4", itemAddedAt = aNow, itemFolder = "1"
        , itemType = "Movie", itemSeries = Nothing, itemSeason = Nothing, itemEpisode = Nothing
        , itemExtraType = Nothing, itemVersions = [] }
      tA = UTCTime (fromGregorian 2024 1 1) 0
      tB = UTCTime (fromGregorian 2024 2 1) 0
      udMap5 = HM.fromList
        [ ("1", emptyUserData { udLastPlayedAt = Just tA, udPlayCount = 3 })
        , ("2", emptyUserData { udLastPlayedAt = Just tB, udPlayCount = 1 })
        , ("3", emptyUserData { udPlayCount = 5 })          -- never played
        ]
      lk5 = flip HM.lookup udMap5
      i1 = mi5 "1" "a"; i2 = mi5 "2" "b"; i3 = mi5 "3" "c"
  check "isUserDataSortKey: the two keys, case-insensitive; others not"
    [True, True, False, False]
    (map isUserDataSortKey ["LastPlayedDate", "PLAYCOUNT", "title", ""])
  check "sortByUserDataField lastPlayedDate: desc, Nothing last"
    ["2", "1", "3"]
    (map itemId (sortByUserDataField "lastPlayedDate" lk5 [i1, i2, i3]))
  check "sortByUserDataField LastPlayedDate case-insensitive"
    ["2", "1", "3"]
    (map itemId (sortByUserDataField "LastPlayedDate" lk5 [i3, i1, i2]))
  check "sortByUserDataField playCount: desc, missing = 0"
    ["3", "1", "2"]
    (map itemId (sortByUserDataField "playcount" lk5 [i1, i2, i3]))
  check "sortByUserDataField unknown key is identity"
    [i3, i1, i2] (sortByUserDataField "title" lk5 [i3, i1, i2])
  check "sortByUserDataField is stable for equal keys"
    ["1", "2", "3"]
    (map itemId (sortByUserDataField "playcount" (const Nothing) [i1, i2, i3]))
  check "parseIdsBody: ItemIds"
    (Right ["1", "2"]) (parseIdsBody (LBSC.pack "{\"ItemIds\":[\"1\",\"2\"]}"))
  check "parseIdsBody: Ids alias"
    (Right ["a"]) (parseIdsBody (LBSC.pack "{\"Ids\":[\"a\"]}"))
  check "parseIdsBody: ItemIds wins over Ids"
    (Right ["x"]) (parseIdsBody (LBSC.pack "{\"ItemIds\":[\"x\"],\"Ids\":[\"y\"]}"))
  check "parseIdsBody: empty object -> []" (Right []) (parseIdsBody (LBSC.pack "{}"))
  check "parseIdsBody: malformed JSON is Left"
    True (either (const True) (const False) (parseIdsBody (LBSC.pack "{nope")))

  -- Milestone 10c (device registry): upsertDevice + Device JSON + devices.json.
  let d1 = Device "aa" "Living Room TV" "Jellyfin Web" "10.9" ["Video","Audio"] (Just "1") (Just aNow)
      d2 = Device "bb" "Phone" "Jellyfin Android" "0.16" [] Nothing Nothing
      d1' = d1 { devAppVersion = "10.10", devCapabilities = ["Video"] }
  check "upsertDevice inserts a new device"
    (Just d1) (HM.lookup "aa" (upsertDevice d1 HM.empty))
  check "upsertDevice updates an existing device, preserving the id"
    (Just d1') (HM.lookup "aa" (upsertDevice d1' (upsertDevice d1 (HM.singleton "bb" d2))))
  check "upsertDevice with an empty id is a no-op (caller mints)"
    HM.empty (upsertDevice (d1 { devId = "" }) HM.empty)
  check "Device JSON roundtrip"
    (Just d1) (Aeson.decode (Aeson.encode d1) :: Maybe Device)
  check "Device FromJSON defaults for missing optional fields"
    (Just ("", "", "", "", [], Nothing, Nothing))
    ((\d -> (devId d, devName d, devAppName d, devAppVersion d, devCapabilities d, devLastUserId d, devLastSeenAt d)) <$>
      (Aeson.decode (LBSC.pack "{}") :: Maybe Device))
  let devFile = "/tmp/fms-test-devices.json"
      devMap = HM.fromList [("aa", d1), ("bb", d2)]
  saveDevices devFile devMap
  loadedDevs <- loadDevices devFile
  check "saveDevices/loadDevices roundtrip" devMap loadedDevs
  missingDevs <- loadDevices "/tmp/fms-test-devices-does-not-exist.json"
  check "loadDevices of a missing file is empty" HM.empty missingDevs
  removeFile devFile

  -- Milestone 4i (/items/:id/Similar): similarItems ranking/exclusion/cap.
  let si base = mi5 base base  -- id == title, path from title
      metaWith genres = sparseMeta { metaGenres = genres }
      simLookup = HM.fromList
        [ ("1", metaWith ["Action", "Sci-Fi"])
        , ("2", metaWith ["action", "Drama"])       -- case-insensitive share: Action
        , ("3", metaWith ["Sci-Fi", "Horror"])       -- shares Sci-Fi only
        , ("4", metaWith ["Drama"])                  -- no share with base
        , ("5", metaWith ["Action", "Sci-Fi", "Drama"])  -- shares 2
        ]
      simItems = [si "1", si "2", si "3", si "4", si "5", si "6"]
      -- item 6 has no metadata -> never matches
  check "similarItems: ranked by shared-genre count desc, self excluded"
    ["5", "2", "3"]
    (map itemId (similarItems (flip HM.lookup simLookup) 12 (si "1") simItems))
  check "similarItems: case-insensitive genre match"
    True ("2" `elem` map itemId (similarItems (flip HM.lookup simLookup) 12 (si "1") simItems))
  check "similarItems: tie on shared count breaks by title asc"
    ["a", "b"]
    (map itemTitle (similarItems (flip HM.lookup (HM.fromList [("x", metaWith ["G"]), ("a", metaWith ["g"]), ("b", metaWith ["G"])])) 12 (si "x") [si "x", si "b", si "a"]))
  check "similarItems: base without metadata -> []"
    [] (map itemId (similarItems (flip HM.lookup simLookup) 12 (si "6") simItems))
  check "similarItems: item without metadata never matches"
    False ("6" `elem` map itemId (similarItems (flip HM.lookup simLookup) 12 (si "1") simItems))
  check "similarItems: limit caps the result"
    ["5", "2"] (map itemId (similarItems (flip HM.lookup simLookup) 2 (si "1") simItems))
  check "similarItems: limit <= 0 means default 12"
    3 (length (similarItems (flip HM.lookup simLookup) 0 (si "1") simItems))

  -- Milestone 11b (server logs endpoint + log level): pushLogEntry ring,
  -- logLevelAllows matrix, LogEntry JSON keys.
  let le :: Int -> LogEntry
      le i = LogEntry aNow "info" ("msg " <> T.pack (show i))
      buf3 = pushLogEntry 3 (pushLogEntry 3 (pushLogEntry 3 [] (le 1)) (le 2)) (le 3)
  check "pushLogEntry appends newest-last and keeps order under cap"
    [1, 2, 3] (map (\e -> read (T.unpack (T.drop 4 (leMessage e))) :: Int) buf3)
  check "pushLogEntry drops the oldest when over cap"
    [2, 3, 4]
    (map (\e -> read (T.unpack (T.drop 4 (leMessage e))) :: Int)
      (pushLogEntry 3 buf3 (le 4)))
  check "pushLogEntry cap 0 keeps nothing"
    ([] :: [LogEntry]) (pushLogEntry 0 [le 1] (le 2))
  check "logRank order debug<info<warn<error, unknown entry = info"
    [0, 1, 2, 3, 1] (map logRank ["debug", "info", "WARN", "error", "bogus"])
  check "logLevelAllows: info config allows info/warn/error, suppresses debug"
    [False, True, True, True] (map (logLevelAllows "info") ["debug", "info", "warn", "error"])
  check "logLevelAllows: error config allows only error"
    [False, False, False, True] (map (logLevelAllows "error") ["debug", "info", "warn", "error"])
  check "logLevelAllows: debug config allows everything"
    [True, True, True, True] (map (logLevelAllows "debug") ["debug", "info", "warn", "error"])
  check "logLevelAllows: unknown config level allows all"
    [True, True] (map (logLevelAllows "bogus") ["debug", "error"])
  check "logLevelAllows: case-insensitive config"
    True (logLevelAllows "Warn" "error")
  check "LogEntry JSON keys Timestamp/Level/Message"
    (Just (True, True, True))
    ((\v -> case v of
        Aeson.Object o -> ( KeyMap.member "Timestamp" o
                           , KeyMap.member "Level" o
                           , KeyMap.member "Message" o )
        _ -> (False, False, False))
      <$> (Aeson.decode (Aeson.encode (le 1)) :: Maybe Aeson.Value))

  -- Milestone 12 (webhooks): eventMatches matrix + Webhook JSON + webhooks.json.
  check "eventMatches: empty list matches everything"
    [True, True] (map (eventMatches []) ["PlaybackStart", "anything"])
  check "eventMatches: \"*\" matches everything"
    [True, True] (map (eventMatches ["*"]) ["PlaybackStart", "Test"])
  check "eventMatches: case-insensitive membership"
    [True, False] (map (eventMatches ["playbackstart", "LibraryChanged"]) ["PlaybackStart", "UserDataSaved"])
  check "eventMatches: no match"
    False (eventMatches ["PlaybackStart"] "LibraryChanged")
  let w1 = Webhook "aa11" "http://example.com/hook" ["PlaybackStart", "*"] True (Just "s3cret") aNow
      w2 = Webhook "bb22" "http://example.com/other" [] False Nothing aNow
  check "Webhook JSON roundtrip"
    (Just w1) (Aeson.decode (Aeson.encode w1) :: Maybe Webhook)
  check "Webhook FromJSON defaults for missing optional fields"
    (Just ([], True, Nothing))
    ((\w -> (whEvents w, whEnabled w, whSecret w)) <$>
      (Aeson.decode (LBSC.pack "{\"Id\":\"x\",\"Url\":\"http://u\",\"AddedAt\":\"2024-01-01T00:00:00Z\"}")
        :: Maybe Webhook))
  let whFile = "/tmp/fms-test-webhooks.json"
      whMap = HM.fromList [("aa11", w1), ("bb22", w2)]
  saveWebhooks whFile whMap
  loadedWh <- loadWebhooks whFile
  check "saveWebhooks/loadWebhooks roundtrip" whMap loadedWh
  missingWh <- loadWebhooks "/tmp/fms-test-webhooks-does-not-exist.json"
  check "loadWebhooks of a missing file is empty" HM.empty missingWh
  removeFile whFile

  -- Milestone 13b (configurable CORS + security headers).
  let acao origins mo =
        BSC.unpack <$> lookup "Access-Control-Allow-Origin" (allowedOriginsHeader origins mo)
  check "parseCorsOrigins: empty -> [\"*\"]"
    ["*"] (parseCorsOrigins "")
  check "parseCorsOrigins: unset-ish whitespace -> [\"*\"]"
    ["*"] (parseCorsOrigins "  , ,")
  check "parseCorsOrigins: comma-separated, trimmed, empties dropped"
    ["https://a.example", "http://b.example:8080"] (parseCorsOrigins " https://a.example , ,http://b.example:8080,")
  check "allowedOriginsHeader: wildcard config -> * even without Origin"
    (Just "*") (acao ["*"] Nothing)
  check "allowedOriginsHeader: wildcard config -> * with Origin"
    (Just "*") (acao ["*"] (Just "https://evil.example"))
  check "allowedOriginsHeader: exact match reflects origin (case-insensitive) + Vary"
    (Just "https://A.example", Just "Origin")
    (acao ["https://a.example"] (Just "https://A.example"),
     BSC.unpack <$> lookup "Vary" (allowedOriginsHeader ["https://a.example"] (Just "https://A.example")))
  check "allowedOriginsHeader: restricted, non-matching origin -> no ACAO"
    Nothing (acao ["https://a.example"] (Just "https://evil.example"))
  check "allowedOriginsHeader: restricted, no Origin header -> no ACAO"
    Nothing (acao ["https://a.example"] Nothing)
  check "securityHeaders: nosniff/SAMEORIGIN/no-referrer, no CSP"
    ([("X-Content-Type-Options","nosniff"),("X-Frame-Options","SAMEORIGIN"),("Referrer-Policy","no-referrer")], False)
    (securityHeaders, any ((== "Content-Security-Policy") . fst) securityHeaders)

  -- Milestone 4j (multi-version media): normalizeVersionTitle, versionRank,
  -- groupVersions, versionParam/versionPathFor.
  check "normalizeVersionTitle strips dot-separated quality tokens + release group"
    "movie name 2019"
    (normalizeVersionTitle "Movie.Name.2019.1080p.BluRay.x264-GRP")
  check "normalizeVersionTitle strips bracketed tokens"
    "movie name" (normalizeVersionTitle "Movie Name [2160p] [WEB-DL]")
  check "normalizeVersionTitle handles 4k/underscore and collapses spaces"
    "movie" (normalizeVersionTitle "Movie_4K__Remux ")
  check "normalizeVersionTitle strips 5.1/2.0 audio tokens (year stays)"
    "movie 2020" (normalizeVersionTitle "Movie.2020.1080p.5.1.AC3")
  check "normalizeVersionTitle plain name unchanged (lowercased)"
    "movie name" (normalizeVersionTitle "Movie Name")
  check "versionRank ordering 8k>4k=2160p>1080p>720p>480p>none"
    [5, 4, 4, 3, 2, 1, 0]
    (map versionRank ["Movie.8k", "Movie.4K", "Movie.2160p", "Movie.1080p", "Movie.720p", "Movie.480p", "Movie"])
  let gv = groupVersions
        [ ("/m/Movie.1080p.mkv", 1000), ("/m/Movie.2160p.mkv", 4000)
        , ("/m/Movie.720p.mkv", 500), ("/m/Other.mp4", 10) ]
  check "groupVersions groups by (dir, normalized title), best first"
    [[("/m/Movie.2160p.mkv", 4000), ("/m/Movie.1080p.mkv", 1000), ("/m/Movie.720p.mkv", 500)], [("/m/Other.mp4", 10)]]
    gv
  check "groupVersions tie on rank breaks by size desc then path asc"
    [[("/m/Movie.1080p.BluRay.mkv", 900), ("/m/Movie.1080p.WEB-DL.mkv", 900), ("/m/Movie.1080p.mkv", 100)]]
    (groupVersions [("/m/Movie.1080p.mkv",100),("/m/Movie.1080p.WEB-DL.mkv",900),("/m/Movie.1080p.BluRay.mkv",900)])
  check "groupVersions separates directories"
    2 (length (groupVersions [("/a/Movie.1080p.mkv",1),("/b/Movie.1080p.mkv",1)]))
  -- milestone 2e: photos/books are never multi-version grouped by the
  -- caller (only video/* goes into groupVersions -- FMS.Modules.Library.Monad);
  -- groupVersions itself groups whatever it is given, so simulate the
  -- caller's filter: photo paths are NOT passed in.
  check "groupVersions leaves same-named photos ungrouped when caller filters to video"
    [[("/m/Movie.1080p.mkv", 100)], [("/m/photo.jpg", 5)], [("/m/photo2.png", 6)]]
    (let isVideo p = "video" `T.isPrefixOf` mimeFor p
         files = [("/m/Movie.1080p.mkv", 100), ("/m/photo.jpg", 5), ("/m/photo2.png", 6)]
     in groupVersions [ (p, s) | (p, s) <- files, isVideo p ]
        ++ [ [(p, s)] | (p, s) <- files, not (isVideo p) ])
  let vpItem = v4 { itemVersions = ["/m/a.1080p.mkv", "/m/a.2160p.mkv"], itemPath = "/m/a.1080p.mkv" }
  check "versionParam: absent -> 0, valid -> n, invalid -> Left"
    [Right 0, Right 2, Left "invalid version parameter"]
    [versionParam [], versionParam [("version","2")], versionParam [("version","x")]]
  check "versionPathFor picks the right file, default primary"
    [Right "/m/a.1080p.mkv", Right "/m/a.2160p.mkv"]
    [versionPathFor vpItem 0, versionPathFor vpItem 1]
  check "versionPathFor out of range -> Left"
    [Left "no such version", Left "version must be >= 0"]
    [versionPathFor vpItem 5, versionPathFor vpItem (-1)]
  check "versionPathFor single-file item: only index 0"
    [Right (itemPath v4), Left "no such version"]
    [versionPathFor v4 0, versionPathFor v4 1]

  -- Milestone 6e (chapter images): fallbackChapters, parseChaptersJson,
  -- ticks, JSON shape, cache path, ffprobe/ffmpeg arg lists.
  check "fallbackChapters: 10 evenly spaced starts over 100s, no titles"
    ([(i, fromIntegral (i * 10) :: Double, Nothing) | i <- [0 .. 9 :: Int]])
    [(chIndex c, chStartSeconds c, chTitle c) | c <- fallbackChapters 100 10]
  check "fallbackChapters: count <= 0 defaults to 10"
    10 (length (fallbackChapters 100 0))
  check "fallbackChapters: duration 0 -> single chapter at 0"
    [Chapter 0 0 Nothing] (fallbackChapters 0 10)
  check "fallbackChapters: negative duration -> single chapter at 0"
    [Chapter 0 0 Nothing] (fallbackChapters (-5) 10)
  check "ticksFromSeconds: 100ns units"
    [0, 15000000, 305000000] (map ticksFromSeconds [0, 1.5, 30.5])
  let probeJson = LBSC.pack
        "{\"format\":{\"duration\":\"125.5\"},\"chapters\":["
        <> LBSC.pack
        "{\"start_time\":\"0.000000\",\"tags\":{\"title\":\"Intro\"}},"
        <> LBSC.pack "{\"start_time\":\"30.5\",\"end_time\":\"60.0\"}]}"
  check "parseChaptersJson: duration + chapters with tags"
    (Right (Just 125.5, [Chapter 0 0 (Just "Intro"), Chapter 1 30.5 Nothing]))
    (parseChaptersJson probeJson)
  check "parseChaptersJson: no chapters -> empty list, numeric duration ok"
    (Right (Just 42, []))
    (parseChaptersJson (LBSC.pack "{\"format\":{\"duration\":42},\"chapters\":[]}"))
  check "parseChaptersJson: malformed JSON -> Left"
    ("Left" :: String) (either (const "Left") (const "Right") (parseChaptersJson (LBSC.pack "{nope")))
  check "chapterToJson: Jellyfin shape, Name present when titled"
    (Just (Aeson.Number 305000000), Just (Aeson.String "Scene"), Just (Aeson.String "2"))
    (case chapterToJson (Chapter 2 30.5 (Just "Scene")) of
       Aeson.Object o ->
         ( KeyMap.lookup (Key.fromText "StartPositionTicks") o
         , KeyMap.lookup (Key.fromText "Name") o
         , KeyMap.lookup (Key.fromText "ImageTag") o )
       _ -> (Nothing, Nothing, Nothing))
  check "chapterToJson: Name omitted when untitled"
    (False, Just (Aeson.Number 0))
    (case chapterToJson (Chapter 0 0 Nothing) of
       Aeson.Object o ->
         ( KeyMap.member (Key.fromText "Name") o
         , KeyMap.lookup (Key.fromText "StartPositionTicks") o )
       _ -> (True, Nothing))
  check "chapterImagePathFor: stateDir/chapters/<id>-<version>-<index>.jpg"
    "/st/chapters/7-1-3.jpg" (chapterImagePathFor "/st" "7" 1 3)
  check "probeChaptersArgs asks for chapters + format as JSON"
    (True, True) (elem "-show_chapters" (probeChaptersArgs "x"), elem "-show_format" (probeChaptersArgs "x"))
  check "chapterImageArgs: -ss start before -i, single mjpeg frame to pipe"
    ["-ss", "30.5", "-i", "/m/a.mkv", "-frames:v", "1", "-q:v", "3", "-f", "image2", "-vcodec", "mjpeg", "pipe:1"]
    (dropWhile (/= "-ss") (chapterImageArgs "/m/a.mkv" 30.5))

  -- Milestone 6f (bitrate caps + hw accel): effectiveBitrateCap,
  -- hwEncoderFor/normalizeHwAccel, transcodeArgsWith, parseFormatBitRate,
  -- decidePlayMethod, policy MaxStreamingBitrate.
  check "effectiveBitrateCap: nothing set -> Nothing"
    Nothing (effectiveBitrateCap Nothing Nothing 0)
  check "effectiveBitrateCap: server cap only"
    (Just 5000000) (effectiveBitrateCap Nothing Nothing 5000000)
  check "effectiveBitrateCap: min of the set caps (user < client < server)"
    (Just 1000000)
    (effectiveBitrateCap (Just 1000000) (Just 8000000) 5000000)
  check "effectiveBitrateCap: zeros are treated as unset"
    Nothing (effectiveBitrateCap (Just 0) Nothing 0)
  check "effectiveBitrateCap: client cap only, 0 ignored"
    (Just 8000000) (effectiveBitrateCap Nothing (Just 8000000) 0)
  check "effectiveBitrateCap: negative server cap ignored"
    (Just 2000000) (effectiveBitrateCap Nothing (Just 2000000) (-1))
  check "normalizeHwAccel: known names kept (case-insensitive), unknown -> none"
    ["vaapi", "nvenc", "qsv", "videotoolbox", "none", "none"]
    (map normalizeHwAccel ["VAAPI", "Nvenc", "qsv", " VideoToolbox ", "cuda", ""])
  check "hwEncoderFor maps accel names to h264 encoders, else Nothing"
    [Just "h264_vaapi", Just "h264_nvenc", Just "h264_qsv", Just "h264_videotoolbox", Nothing]
    (map hwEncoderFor ["vaapi", "nvenc", "qsv", "videotoolbox", "none"])
  check "transcodeArgsWith: no accel, no cap == transcodeArgs"
    (transcodeArgs "/m/a.mkv") (transcodeArgsWith "none" Nothing "/m/a.mkv")
  check "transcodeArgsWith: hwaccel adds -hwaccel before -i and swaps encoder"
    ["-hwaccel", "vaapi", "-i", "/m/a.mkv", "-c:v", "h264_vaapi"]
    (take 6 (drop 2 (transcodeArgsWith "vaapi" Nothing "/m/a.mkv")))
  check "transcodeArgsWith: libx264 keeps -preset veryfast"
    True ("-preset" `elem` transcodeArgsWith "none" Nothing "/m/a.mkv")
  check "transcodeArgsWith: hw encoder drops the x264-only -preset"
    False ("-preset" `elem` transcodeArgsWith "nvenc" Nothing "/m/a.mkv")
  check "transcodeArgsWith: bitrate cap adds -b:v/-maxrate/-bufsize(2x) before -c:a"
    ["-b:v", "1000000", "-maxrate", "1000000", "-bufsize", "2000000", "-c:a", "aac", "-movflags", "frag_keyframe+empty_moov", "-f", "mp4", "pipe:1"]
    (dropWhile (/= "-b:v") (transcodeArgsWith "none" (Just 1000000) "/m/a.mkv"))
  check "transcodeArgsWith: no cap -> no -b:v"
    False ("-b:v" `elem` transcodeArgsWith "none" Nothing "/m/a.mkv")
  check "parseFormatBitRate: string bit_rate"
    (Just 91767)
    (parseFormatBitRate (LBSC.pack "{\"format\":{\"bit_rate\":\"91767\",\"duration\":\"20.0\"}}"))
  check "parseFormatBitRate: numeric bit_rate, missing -> Nothing, bad JSON -> Nothing"
    [Just 5000, Nothing, Nothing]
    [ parseFormatBitRate (LBSC.pack "{\"format\":{\"bit_rate\":5000}}")
    , parseFormatBitRate (LBSC.pack "{\"format\":{\"duration\":\"1\"}}")
    , parseFormatBitRate (LBSC.pack "{nope") ]
  check "probeBitrateArgs asks only for the format section"
    (True, False) (elem "-show_format" (probeBitrateArgs "x"), elem "-show_streams" (probeBitrateArgs "x"))
  check "decidePlayMethod: direct playable + no cap -> DirectPlay"
    "DirectPlay" (decidePlayMethod True Nothing Nothing)
  check "decidePlayMethod: bitrate under cap -> DirectPlay, over -> Transcode"
    ["DirectPlay", "Transcode"]
    [decidePlayMethod True (Just 4000000) (Just 5000000), decidePlayMethod True (Just 9000000) (Just 5000000)]
  check "decidePlayMethod: unknown source bitrate passes the cap"
    "DirectPlay" (decidePlayMethod True Nothing (Just 1000))
  check "decidePlayMethod: not direct playable -> Transcode"
    "Transcode" (decidePlayMethod False (Just 100) Nothing)
  check "UserPolicy JSON roundtrip keeps MaxStreamingBitrate"
    (Just (Just 2500000))
    (policyMaxStreamingBitrate <$>
      (Aeson.decode (LBSC.pack "{\"MaxParentalRating\":null,\"BlockedTags\":[],\"AccessSchedules\":[],\"MaxStreamingBitrate\":2500000}") :: Maybe UserPolicy))
  check "UserPolicy FromJSON without MaxStreamingBitrate (pre-6f) defaults to Nothing"
    (Just Nothing)
    (policyMaxStreamingBitrate <$>
      (Aeson.decode (LBSC.pack "{\"MaxParentalRating\":\"PG\",\"BlockedTags\":[],\"AccessSchedules\":[]}") :: Maybe UserPolicy))
  check "PolicyPatch sets/clears MaxStreamingBitrate"
    [Just 3000000, Nothing]
    [ policyMaxStreamingBitrate (applyPolicyPatch emptyUserPolicy
        (either (const (PolicyPatch Nothing Nothing Nothing Nothing)) id
          (Aeson.eitherDecode (LBSC.pack "{\"MaxStreamingBitrate\":3000000}"))))
    , policyMaxStreamingBitrate (applyPolicyPatch
        (emptyUserPolicy { policyMaxStreamingBitrate = Just 1 })
        (PolicyPatch Nothing Nothing Nothing (Just Nothing))) ]
  check "PolicyPatch JSON null clears MaxStreamingBitrate (.:? would collapse null)"
    (Just (Just Nothing))
    (either (const Nothing) (Just . ppMaxBitrate)
      (Aeson.eitherDecode (LBSC.pack "{\"MaxStreamingBitrate\":null}") :: Either String PolicyPatch))

  -- Milestone 3g (DASH): dashArgs shape, bitrate cap args, isValidDashSegName
  -- traversal matrix, dashDirFor path shape.
  let dargs = dashArgs "/m/a.mkv" "/st/dash/1-0/manifest.mpd"
  check "dashArgs: dash muxer, 4s segments, template names"
    (True, True, True, True)
    ( elem "-f" dargs && take 1 (drop 1 (dropWhile (/= "-f") dargs)) == ["dash"]
    , take 1 (drop 1 (dropWhile (/= "-seg_duration") dargs)) == ["4"]
    , "init-stream$Number$.m4s" `elem` dargs
    , "chunk-stream$Number$-$Number$.m4s" `elem` dargs )
  check "dashArgs: output mpd is the last arg, no bitrate args without cap"
    (True, False)
    ( last dargs == "/st/dash/1-0/manifest.mpd"
    , "-b:v" `elem` dargs )
  check "dashArgsWith: bitrate cap adds -b:v/-maxrate/-bufsize(2x)"
    ["-b:v", "1000000", "-maxrate", "1000000", "-bufsize", "2000000"]
    (take 6 (dropWhile (/= "-b:v") (dashArgsWith (Just 1000000) "/m/a.mkv" "/o/manifest.mpd")))
  check "dashArgsWith: Nothing/0 cap -> no -b:v"
    (False, False)
    ( "-b:v" `elem` dashArgsWith Nothing "/m/a.mkv" "/o.mpd"
    , "-b:v" `elem` dashArgsWith (Just 0) "/m/a.mkv" "/o.mpd" )
  check "isValidDashSegName: accepts init/media segment names"
    [True, True, True]
    (map isValidDashSegName ["init-stream0.m4s", "chunk-stream0-00001.m4s", "a-b_c.d"])
  check "isValidDashSegName: rejects traversal, separators, empty"
    [False, False, False, False, False]
    (map isValidDashSegName ["..", "../manifest.mpd", "a/b.m4s", "", "a b.m4s"])
  check "dashDirFor: stateDir/dash/<id>-<version>"
    "/st/dash/7-2"
    (dashDirFor Config
      { configLibraryPath = "/lib", configPort = 8080, configStateDir = "/st"
      , configRequireAuth = False, configServerName = "fms"
      , configFollowSymlinks = False, configTmdbApiKey = ""
      , configRateLimit = Nothing, configLogLevel = "info"
      , configCorsOrigins = ["*"], configSecurityHeaders = True
      , configPasswordMinLength = 0, configMaxBitrate = 0, configHwAccel = "none"
      , configWatch = False, configMaxTranscodes = 2, configFfmpegTimeoutSec = 30
      , configTlsCert = Nothing, configTlsKey = Nothing
      , configFetchImages = False, configTmdbBaseUrl = "https://api.themoviedb.org/3"
      , configTmdbImageBaseUrl = "https://image.tmdb.org/t/p", configTmdbImageSize = "w500"
      } "7" 2)

  -- Milestone 2d (filesystem watching): eventsPending debounce predicate
  -- and the media-file filter (pure helpers, platform-independent).
  let wNow = UTCTime (fromGregorian 2024 6 1) 43200  -- 2024-06-01 12:00:00
      wOld = UTCTime (fromGregorian 2024 6 1) 43000  -- 100s earlier
  check "eventsPending: event inside the window -> True"
    True (eventsPending [(wNow, "/m/a.mp4")] wNow 2.0)
  check "eventsPending: event older than the window -> False"
    False (eventsPending [(wOld, "/m/a.mp4")] wNow 2.0)
  check "eventsPending: empty list -> False"
    False (eventsPending [] wNow 2.0)
  check "eventsPending: any event inside counts"
    True (eventsPending [(wOld, "/x"), (wNow, "/m/b.mkv")] wNow 2.0)
  check "watchMediaFile: media extensions pass, others do not (2e: photos/books are media too)"
    [True, True, False, False, True, True]
    (map watchMediaFile ["/m/a.MP4", "b.mkv", "notes.txt", "data.bin", "cover.jpg", "book.epub"])

  -- Milestone 11c: ScanProgress event payload shapes (docs/todo.org
  -- section 2).
  let evScan  = scanProgressData "Scanning" [("FolderId", Aeson.String "1"), ("Folder", Aeson.String "Movies")]
      evDone  = scanProgressData "Completed" [("Added", Aeson.Number 2), ("Changed", Aeson.Number 1), ("Removed", Aeson.Number 0)]
      evFail  = scanProgressData "Failed" [("Error", Aeson.String "boom")]
      evRaw v = LBS.toStrict (Aeson.encode v)
  check "scanProgressData Scanning has Status + Folder fields"
    [True, True, True]
    (map (`BSC.isInfixOf` evRaw evScan)
      [ "\"Status\":\"Scanning\"", "\"FolderId\":\"1\"", "\"Folder\":\"Movies\"" ])
  check "scanProgressData Completed has Status + Added/Changed/Removed counts"
    [True, True, True, True]
    (map (`BSC.isInfixOf` evRaw evDone)
      [ "\"Status\":\"Completed\"", "\"Added\":2", "\"Changed\":1", "\"Removed\":0" ])
  check "scanProgressData Failed has Status + Error"
    [True, True]
    (map (`BSC.isInfixOf` evRaw evFail) [ "\"Status\":\"Failed\"", "\"Error\":\"boom\"" ])
  check "scanProgressData with no extra fields is just Status"
    "{\"Status\":\"Scanning\"}" (BSC.unpack (evRaw (scanProgressData "Scanning" [])))

  -- Milestone 6g (image cache tags + backdrop index): imageTag format and
  -- determinism, selectBackdrop bounds/sorting.
  check "imageTag: <size>-<mtimeEpoch> format"
    "1234-1717200000" (imageTag 1234 1717200000)
  check "imageTag: deterministic and size+mtime sensitive"
    (True, False, False)
    ( imageTag 10 20 == imageTag 10 20
    , imageTag 10 20 == imageTag 11 20
    , imageTag 10 20 == imageTag 10 21 )
  check "selectBackdrop: picks sorted entry by index"
    (Just "/d/Backdrop1.jpg")
    (selectBackdrop ["/d/Backdrop2.jpg", "/d/Backdrop.jpg", "/d/Backdrop1.jpg"] 1)
  check "selectBackdrop: index 0 is the first sorted file"
    (Just "/d/Backdrop.jpg") (selectBackdrop ["/d/Backdrop1.jpg", "/d/Backdrop.jpg"] 0)
  check "selectBackdrop: out of range / negative / empty -> Nothing"
    [Nothing, Nothing, Nothing]
    [ selectBackdrop ["/d/Backdrop.jpg"] 1
    , selectBackdrop ["/d/Backdrop.jpg"] (-1)
    , selectBackdrop [] 0 ]

  -- Milestone 6h (MediaSegments): validSegmentType, ticks<->seconds,
  -- validateSegment matrix, JSON roundtrip, persistence roundtrip.
  check "validSegmentType: recognised types, case-insensitive"
    [True, True, True, False, False]
    (map validSegmentType ["Intro", "OUTRO", "preview", "Ad", ""])
  check "ticksToSeconds/secondsToTicks roundtrip"
    [0, 1.5, 20] (map (ticksToSeconds . secondsToTicks) [0, 1.5, 20 :: Double])
  check "secondsToTicks: 100ns units"
    [0, 15000000, 200000000] (map secondsToTicks [0, 1.5, 20 :: Double])
  let segOk = MediaSegment "s1" "1" "Intro" 0 50000000
      segBadType = segOk { segType = "Ad" }
      segBadRange = segOk { segEndTicks = 0 }
      segNeg = segOk { segStartTicks = -1 }
  check "validateSegment: valid passes"
    (Right segOk) (validateSegment segOk)
  check "validateSegment: bad type -> Left"
    True (either (const True) (const False) (validateSegment segBadType))
  check "validateSegment: end <= start -> Left"
    True (either (const True) (const False) (validateSegment segBadRange))
  check "validateSegment: negative start -> Left"
    True (either (const True) (const False) (validateSegment segNeg))
  check "MediaSegment JSON roundtrip"
    (Just segOk) (Aeson.decode (Aeson.encode segOk) :: Maybe MediaSegment)
  check "MediaSegment FromJSON defaults for missing keys"
    (Just (MediaSegment "" "" "" 0 0))
    (Aeson.decode (LBSC.pack "{}") :: Maybe MediaSegment)
  let segFile = "/tmp/fms-test-segments.json"
      segMap = HM.fromList [("1", [segOk]), ("2", [segOk { segId = "s2", segItemId = "2", segType = "Outro" }])]
  saveSegments segFile segMap
  loadedSegs <- loadSegments segFile
  check "saveSegments/loadSegments roundtrip" segMap loadedSegs
  missingSegs <- loadSegments "/tmp/fms-test-segments-does-not-exist.json"
  check "loadSegments of a missing file is empty" HM.empty missingSegs
  removeFile segFile

  -- Milestone 1d (CLI args + config validation): parseCliArgs matrix and
  -- validateConfig matrix.
  check "parseCliArgs: value flags (space and = forms)"
    (Right [("port","8080"),("library-path","/m"),("state-dir","/s"),("log-level","debug")])
    (parseCliArgs ["--port","8080","--library-path=/m","--state-dir","/s","--log-level=debug"])
  check "parseCliArgs: bare boolean flag means true, =false works"
    (Right [("require-auth","true"),("watch","false"),("follow-symlinks","true")])
    (parseCliArgs ["--require-auth","--watch=false","--follow-symlinks"])
  check "parseCliArgs: --no-security-headers maps to security-headers=false"
    (Right [("security-headers","false")]) (parseCliArgs ["--no-security-headers"])
  check "parseCliArgs: --help / --version"
    (Right [("help","")]) (parseCliArgs ["--help"])
  check "parseCliArgs: all value flags"
    (Right [("server-name","x"),("tmdb-api-key","k"),("rate-limit","5"),("cors-origins","a,b"),("password-min-length","8"),("max-bitrate","1000"),("hwaccel","vaapi")])
    (parseCliArgs ["--server-name","x","--tmdb-api-key=k","--rate-limit","5","--cors-origins","a,b","--password-min-length=8","--max-bitrate","1000","--hwaccel","vaapi"])
  check "parseCliArgs: unknown option -> Left"
    True (either (const True) (const False) (parseCliArgs ["--frobnicate"]))
  check "parseCliArgs: missing value -> Left"
    True (either (const True) (const False) (parseCliArgs ["--port"]))
  let cfg1d = Config
        { configLibraryPath = "/lib", configPort = 8080, configStateDir = "/st"
        , configRequireAuth = False, configServerName = "fms"
        , configFollowSymlinks = False, configTmdbApiKey = ""
        , configRateLimit = Nothing, configLogLevel = "INFO"
        , configCorsOrigins = ["*"], configSecurityHeaders = True
        , configPasswordMinLength = 0, configMaxBitrate = 0, configHwAccel = "VAAPI"
        , configWatch = False, configMaxTranscodes = 2, configFfmpegTimeoutSec = 30
        , configTlsCert = Nothing, configTlsKey = Nothing
        , configFetchImages = False, configTmdbBaseUrl = "https://api.themoviedb.org/3"
        , configTmdbImageBaseUrl = "https://image.tmdb.org/t/p", configTmdbImageSize = "w500"
        }
      badPort p = cfg1d { configPort = p }
      isLeft' :: Either a b -> Bool
      isLeft' = either (const True) (const False)
  check "validateConfig: ok config passes and normalises log level + hwaccel"
    (Right (cfg1d { configLogLevel = "info", configHwAccel = "vaapi" }))
    (validateConfig cfg1d)
  check "validateConfig: ports 0/65536/70000 rejected, 1 and 65535 ok"
    ([True, True, True], [False, False])
    ( map isLeft' [validateConfig (badPort 0), validateConfig (badPort 65536), validateConfig (badPort 70000)]
    , map isLeft' [validateConfig (badPort 1), validateConfig (badPort 65535)] )
  check "validateConfig: empty library path / state dir rejected"
    [True, True] [isLeft' (validateConfig (cfg1d { configLibraryPath = "" })),
                 isLeft' (validateConfig (cfg1d { configStateDir = "" }))]
  check "validateConfig: bad log level rejected, case-insensitive ok"
    [True, False] [isLeft' (validateConfig (cfg1d { configLogLevel = "verbose" })),
                  isLeft' (validateConfig (cfg1d { configLogLevel = "Warn" }))]
  check "validateConfig: empty cors list and negative numbers rejected"
    [True, True, True]
    [ isLeft' (validateConfig (cfg1d { configCorsOrigins = [] }))
    , isLeft' (validateConfig (cfg1d { configPasswordMinLength = (-1) }))
    , isLeft' (validateConfig (cfg1d { configMaxBitrate = (-5) })) ]

  -- Milestone 4k (manual metadata editing + field locking, docs/todo.org
  -- section 3): fieldNameFor, applyManualPatch, applyProviderMerge, and the
  -- pre-4k metadata.json default for lockedFields.
  let mkPatch kvs = KeyMap.fromList [(Key.fromText k, v) | (k, v) <- kvs]
      base4k = emptyMetadata { metaTitle = Just "Old Title", metaGenres = ["Drama"], metaLockedFields = ["title"] }
  check "fieldNameFor maps known keys case-insensitively, rejects others"
    [Just "title", Just "genres", Just "officialRating", Nothing, Nothing]
    [ fieldNameFor "Title", fieldNameFor "GENRES", fieldNameFor "officialRating"
    , fieldNameFor "source", fieldNameFor "bogus" ]
  check "applyManualPatch sets unlocked fields, skips locked ones"
    (base4k { metaGenres = ["Horror"], metaYear = Just 1999 })
    (applyManualPatch base4k (mkPatch
      [ ("genres", Aeson.toJSON (["Horror"] :: [T.Text]))
      , ("title", Aeson.toJSON ("New" :: T.Text))    -- locked: skipped
      , ("year", Aeson.toJSON (1999 :: Int))
      ]))
  check "applyManualPatch ignores unknown keys"
    base4k (applyManualPatch base4k (mkPatch [("bogus", Aeson.toJSON (1 :: Int))]))
  check "applyManualPatch lockedFields key is always settable"
    (base4k { metaLockedFields = ["title", "genres"] })
    (applyManualPatch base4k (mkPatch [("lockedFields", Aeson.toJSON (["title", "genres"] :: [T.Text]))]))
  check "applyManualPatch JSON null clears a Maybe field"
    (emptyMetadata { metaOverview = Nothing })
    (applyManualPatch (emptyMetadata { metaOverview = Just "x" }) (mkPatch [("overview", Aeson.Null)]))
  check "applyProviderMerge keeps locked old fields, takes new elsewhere, carries locks"
    (emptyMetadata { metaTitle = Just "Old Title", metaOverview = Just "New plot"
                   , metaGenres = ["Action"], metaLockedFields = ["title"] })
    (applyProviderMerge base4k (emptyMetadata
      { metaTitle = Just "Prov Title", metaOverview = Just "New plot", metaGenres = ["Action"] }))
  check "MediaMetadata FromJSON without lockedFields defaults to [] (pre-4k metadata.json)"
    []
    (metaLockedFields (either (const emptyMetadata) id
      (Aeson.eitherDecode "{\"title\":\"x\"}" :: Either String MediaMetadata)))
  check "MediaMetadata JSON roundtrip carries lockedFields"
    (Just (base4k { metaLockedFields = ["year"] }))
    (Aeson.decode (Aeson.encode (base4k { metaLockedFields = ["year"] })) :: Maybe MediaMetadata)

  -- Milestone 15 (ffmpeg process management, docs/todo.org section 13):
  -- the slot semaphore helper and the input-timeout ffmpeg args.
  do sem <- newQSemN 1
     slot1 <- tryAcquireSlot sem 0
     slot2 <- tryAcquireSlot sem 1  -- slot held: waits 1s -> False
     signalQSemN sem 1
     slot3 <- tryAcquireSlot sem 0  -- released: immediate True
     check "tryAcquireSlot: acquire when free, timeout when held, ok after release"
       (True, False, True) (slot1, slot2, slot3)
  check "ffmpegTimeoutArgs: <=0 -> [], network input -> -rw_timeout us, local file -> []"
    ([], ["-rw_timeout", "30000000"], [], ["-rw_timeout", "5000000"])
    ( ffmpegTimeoutArgs 0 "http://x/y.mp4"
    , ffmpegTimeoutArgs 30 "http://x/y.mp4"
    , ffmpegTimeoutArgs 30 "/lib/movie.mp4"
    , ffmpegTimeoutArgs 5 "rtsp://cam/stream" )
  check "validateConfig: maxTranscodes / ffmpegTimeout below 1 rejected"
    [True, True]
    [ isLeft' (validateConfig (cfg1d { configMaxTranscodes = 0 }))
    , isLeft' (validateConfig (cfg1d { configFfmpegTimeoutSec = 0 })) ]

  -- Milestone 13d (HTTPS/TLS termination, docs/todo.org section 13):
  -- CLI flags for --tls-cert/--tls-key and the both-or-neither validation.
  check "parseCliArgs: --tls-cert/--tls-key in both forms"
    (Right [("tls-cert", "/c.pem"), ("tls-key", "/k.pem")])
    (parseCliArgs ["--tls-cert", "/c.pem", "--tls-key=/k.pem"])
  check "validateConfig: only one of TLS cert/key -> Left; both -> ok; neither -> ok"
    [True, False, False]
    [ isLeft' (validateConfig (cfg1d { configTlsCert = Just "/c.pem" }))
    , isLeft' (validateConfig (cfg1d { configTlsCert = Just "/c.pem", configTlsKey = Just "/k.pem" }))
    , isLeft' (validateConfig cfg1d) ]

  -- Milestone 16 (security hardening, docs/todo.org section 13):
  -- validateFolderPath rejects empty/relative/traversal/null-byte paths.
  let isRight' :: Either a b -> Bool
      isRight' = either (const False) (const True)
  check "validateFolderPath: absolute paths ok (trailing slash kept normalised)"
    [Right "/mnt/movies", Right "/mnt/movies/", Right "/"]
    [validateFolderPath "/mnt/movies", validateFolderPath "/mnt/movies/", validateFolderPath "/"]
  check "validateFolderPath: relative, empty, .. segment, null byte rejected"
    [False, False, False, False, False]
    [ isRight' (validateFolderPath "movies")
    , isRight' (validateFolderPath "")
    , isRight' (validateFolderPath "/mnt/../etc")
    , isRight' (validateFolderPath "../etc")
    , isRight' (validateFolderPath "/mnt/mo\0vies")
    ]

  -- Milestone 17a (jellyfin-web compat): case-insensitive path segments,
  -- case-insensitive query param names, ParentId filter.
  check "normalizeSegments lowercases path segments"
    ["items", "videos", "1", "main.m3u8"]
    (normalizeSegments ["Items", "Videos", "1", "MAIN.M3U8"])
  check "matchSegments: Lit compares case-insensitively, captures keep case"
    (Just [("id", "AbC")], True)
    ( matchSegments [Lit "Items", Capture "id"] ["ITEMS", "AbC"]
    , matchSegments [Lit "items"] ["ITEMS"] == Just [] )
  check "isOpenPath is case-insensitive"
    [True, True, False]
    [ isOpenPath ["system", "info", "public"], isOpenPath ["System", "Info", "Public"]
    , isOpenPath ["items"] ]
  check "normalizeQueryParams lowercases names, keeps values and order"
    [("filters", "IsPlayed"), ("includeitemtypes", "Movie,Episode"), ("sortby", "SortName")]
    (normalizeQueryParams [("Filters", "IsPlayed"), ("IncludeItemTypes", "Movie,Episode"), ("SortBy", "SortName")])
  check "queryParams: PascalCase WAI query names are folded (value untouched)"
    [("parentid", "1"), ("sortby", "Title")]
    (queryParams [("ParentId", Just "1"), ("SortBy", Just "Title")])
  let m17a = testItem
      items17 = [m17a { itemId = "a", itemFolder = "1" }, m17a { itemId = "b", itemFolder = "2" }]
  check "filterByParent: keeps only the given folder, empty/absent -> identity"
    (["a"], ["a", "b"], ["a", "b"])
    ( map itemId (filterByParent [("parentid", "1")] items17)
    , map itemId (filterByParent [("parentid", "")] items17)
    , map itemId (filterByParent [] items17) )

  -- Milestone 17b (jellyfin-web compat): PascalCase JSON keys for MediaItem
  -- and the public User; the internal persistence JSON is unchanged.
  let jsonKeys v = case v of
        Aeson.Object o -> sort (map Key.toText (KeyMap.keys o))
        _              -> []
      u17b = User "1" "admin" "hash" True [] Nothing emptyUserPolicy True False
  check "MediaItem ToJSON uses exactly the PascalCase Jellyfin key set"
    [ "DateCreated", "EpisodeNumber", "ExtraType", "FolderId", "Id", "MimeType"
    , "Name", "Path", "SeasonNumber", "SeriesName", "Size", "Type", "Versions" ]
    (jsonKeys (Aeson.toJSON (testItem { itemId = "1" })))
  check "User public ToJSON uses exactly the PascalCase key set"
    [ "AllowedFolders", "Enabled", "HasPassword", "Id", "IsAdministrator"
    , "MustChangePassword", "Name", "Policy" ]
    (jsonKeys (Aeson.toJSON u17b))
  check "User internal JSON keeps the lowercase persistence keys (unchanged)"
    True (all (`BSC.isInfixOf` LBS.toStrict (Aeson.encode (toInternalJSON u17b)))
      [ "\"userId\":", "\"userName\":", "\"passwordHash\":", "\"isAdmin\":"
      , "\"allowedFolders\":", "\"mustChangePassword\":" ])
  check "User internal JSON roundtrip still decodes"
    (Just u17b) (Aeson.decode (Aeson.encode (toInternalJSON u17b)) :: Maybe User)
  let iv17b = Aeson.toJSON (testItem { itemId = "1" })
  check "projectFields fields=Name and fields=name both keep Id+Name (case-insensitive)"
    (["Id", "Name"], ["Id", "Name"])
    (jsonKeys (projectFields ["Name"] iv17b), jsonKeys (projectFields ["name"] iv17b))

  -- Milestone 17c (jellyfin-web compat part 3): Views builder, NextUp,
  -- Latest, Progress PascalCase keys.
  let f1 = MediaFolder "1" "Media" "/lib" "mixed"
      f2 = MediaFolder "2" "Movies" "/movies" "movies"
      vj = LBSC.unpack (Aeson.encode (viewsFromFolders [f1, f2]))
  check "viewsFromFolders: CollectionFolder view objects with PascalCase keys"
    [True, True, True, True]
    [ "\"Name\":\"Media\"" `isInfixOf` vj
    , "\"Id\":\"2\"" `isInfixOf` vj
    , "\"Type\":\"CollectionFolder\"" `isInfixOf` vj
    , "\"CollectionType\":\"movies\"" `isInfixOf` vj ]
  let ep ser n = testItem { itemId = "e" <> ser <> T.pack (show n), itemType = "Episode"
                         , itemSeries = Just ser, itemSeason = Just 1
                         , itemEpisode = Just n, itemTitle = ser <> " ep" <> T.pack (show n) }
      eps17 = [ep "S" 1, ep "S" 2, ep "T" 1]
      udPlayed1 = \iid -> if iid == "eS1" then Just (emptyUserData { udPlayed = True }) else Nothing
  check "nextUpEpisodes picks the first UNPLAYED episode per series"
    ["eS2", "eT1"] (map itemId (nextUpEpisodes udPlayed1 eps17))
  check "nextUpEpisodes with empty lookup -> first episode of each series"
    ["eS1", "eT1"] (map itemId (nextUpEpisodes (const Nothing) eps17))
  check "nextUpEpisodes skips a fully-played series"
    ["eT1"] (map itemId (nextUpEpisodes (\iid -> if iid == "eS1" || iid == "eS2" then Just (emptyUserData { udPlayed = True }) else Nothing) eps17))
  let t17a = UTCTime (fromGregorian 2020 1 1) 0
      t17b = UTCTime (fromGregorian 2021 1 1) 0
      t17c = UTCTime (fromGregorian 2022 1 1) 0
      la17 = [ testItem { itemId = "a", itemAddedAt = t17a, itemType = "Episode" }
           , testItem { itemId = "b", itemAddedAt = t17c, itemType = "Movie" }
           , testItem { itemId = "c", itemAddedAt = t17b, itemType = "Audio" } ]
  check "latestItems: DateCreated desc, default cap 12, Limit cap, IncludeItemTypes"
    (["b", "c", "a"], ["b", "c"], ["b"])
    ( map itemId (latestItems [] 0 la17)
    , map itemId (latestItems [] 2 la17)
    , map itemId (latestItems [("includeItemTypes", "Movie")] 0 la17) )
  let p17 = Progress 5 10 (UTCTime (fromGregorian 2024 1 1) 0)
      pj = LBS.toStrict (Aeson.encode p17)
  check "Progress ToJSON uses PascalCase keys (17c)"
    True (all (`BSC.isInfixOf` pj)
      [ "\"PositionSeconds\":", "\"DurationSeconds\":", "\"UpdatedAt\":" ])
  check "Progress FromJSON accepts new AND old (pre-17c stats.json) keys"
    (Just p17, Just p17)
    ( Aeson.decode (Aeson.encode p17) :: Maybe Progress
    , Aeson.decode (LBSC.pack "{\"currentTime\":5,\"duration\":10,\"updatedAt\":\"2024-01-01T00:00:00Z\"}") :: Maybe Progress )

  -- SceneDetect (intro/outro auto-detection parsers + proposal)
  let bl1 = "[blackdetect @ 0x55] black_start:0 black_end:3.04 black_duration:3.04"
      bl2 = "[blackdetect @ 0x55] black_start:13 black_end:16 black_duration:3"
      blDur = "[blackdetect @ 0x55] black_start:5 black_duration:2.5"
      blGarbage = "[blackdetect @ 0x55] config in time_base: no line here"
  check "parseBlackdetect: start/end lines, duration fallback, garbage ignored"
    ([(0, 3.04), (13, 16), (5, 7.5)], 3)
    (parseBlackdetect (unlines [bl1, bl2, blDur, blGarbage, "random noise"])
    , length (parseBlackdetect (unlines [bl1, bl2, blDur, blGarbage, "random noise"])))
  let sl1 = "[silencedetect @ 0x77] silence_start: 1.5"
      sl2 = "[silencedetect @ 0x77] silence_end: 4.2 | silence_duration: 2.7"
      sl3 = "[silencedetect @ 0x77] silence_start: 9.0"
      sl4 = "[silencedetect @ 0x77] silence_start: 20"
      sl5 = "[silencedetect @ 0x77] silence_end: 22.5 | silence_duration: 2.5"
  check "parseSilencedetect: paired start/end, trailing unpaired start dropped"
    ([(1.5, 4.2), (20, 22.5)], 2)
    (parseSilencedetect (unlines [sl1, sl2, sl4, sl5, sl3, "garbage"])
    , length (parseSilencedetect (unlines [sl1, sl2, sl4, sl5, sl3, "garbage"])))
  let slDurOnly = "[silencedetect @ 0x77] silence_start: 8 silence_duration: 1.25"
  check "parseSilencedetect: silence_duration fallback when no end line"
    [(8, 9.25)] (parseSilencedetect slDurOnly)
  check "mergeIntervals: overlap, adjacency (gap < 0.5s), disjoint"
    ([(0, 3.2), (5, 6), (10, 12)], [(0, 1), (2, 3)], [(0, 1), (5, 6)])
    ( mergeIntervals [(0, 2), (2.2, 3.2), (5, 6), (10, 12)]
    , mergeIntervals [(0, 1), (2, 3)]
    , mergeIntervals [(5, 6), (0, 1)] )
  check "intervalsOverlap matrix"
    [True, True, False, False, False]
    [ intervalsOverlap (0, 3) (2, 5), intervalsOverlap (2, 5) (0, 3)
    , intervalsOverlap (0, 3) (3, 5), intervalsOverlap (0, 3) (4, 5)
    , intervalsOverlap (5, 6) (0, 3) ]
  let blacks16 = [(0, 3), (13, 16)]
  check "proposeSegments: black intro 0-3 + outro 13-16 with duration 16"
    (Just (0, 3), Just (13, 16)) (proposeSegments 16 blacks16 [])
  check "proposeSegments: no intervals -> (Nothing, Nothing)"
    (Nothing, Nothing) (proposeSegments 16 [] [])
  check "proposeSegments: silence-only intro works"
    (Just (0, 2.5), Nothing) (proposeSegments 16 [] [(0, 2.5), (10, 11)])
  check "proposeSegments: interval starting at 1s (within 2s) counts as intro"
    (Just (1, 4), Nothing) (proposeSegments 16 [(1, 4)] [])
  check "proposeSegments: intro longer than 300s is capped at 300"
    (Just (0, 300), Nothing) (proposeSegments 400 [(0, 350)] [])
  check "proposeSegments: outro longer than 300s is capped (start = duration-300)"
    (Nothing, Just (100, 400)) (proposeSegments 400 [(50, 400)] [])
  check "proposeSegments: duration <= 0 -> (Nothing, Nothing)"
    (Nothing, Nothing) (proposeSegments 0 blacks16 blacks16)
  check "proposeSegments: mid-movie black interval is neither intro nor outro"
    (Nothing, Nothing) (proposeSegments 16 [(6, 8)] [])
  check "blackdetectArgs / silencedetectArgs shapes"
    (True, True, True, True)
    ( "blackdetect=d=0.05:pix_th=0.10" `elem` blackdetectArgs "/m/x.mp4"
    , "-f" `elem` blackdetectArgs "/m/x.mp4" && "null" `elem` blackdetectArgs "/m/x.mp4"
    , "silencedetect=n=-35dB:d=0.4" `elem` silencedetectArgs "/m/x.mp4"
    , "-i" `elem` silencedetectArgs "/m/x.mp4" && "/m/x.mp4" `elem` silencedetectArgs "/m/x.mp4" )
  check "parseFormatDuration: ffprobe -show_format fixture"
    (Just 16.5, Nothing)
    ( parseFormatDuration (LBSC.pack "{\"format\":{\"duration\":\"16.500000\",\"bit_rate\":\"123\"}}")
    , parseFormatDuration (LBSC.pack "{\"format\":{\"bit_rate\":\"123\"}}") )

  -- Item tags API: mergeTags / removeTag / attachTags / provider-merge union
  check "mergeTags: existing order kept, new appended, dedupe case-insensitive"
    (["favorite", "hd", "Criterion"], ["favorite"], ["a", "b"])
    ( mergeTags ["favorite", "hd"] ["HD", "Criterion", "favorite"]
    , mergeTags ["favorite"] []
    , mergeTags [] ["a", "b"] )
  check "removeTag: case-insensitive removal, missing tag is a no-op"
    (["favorite"], ["a", "b"], ["b"])
    ( removeTag "HD" ["favorite", "hd"]
    , removeTag "zzz" ["a", "b"]
    , removeTag "A" ["a", "b"] )
  let tagObj = Aeson.object [Key.fromText "Id" Aeson..= ("1" :: T.Text)]
      tagV = attachTags ["favorite", "hd"] tagObj
      tagJ = LBS.toStrict (Aeson.encode tagV)
  check "attachTags inserts a Tags array into an item object"
    [True, True] [ "\"Tags\":[\"favorite\",\"hd\"]" `BSC.isInfixOf` tagJ
                , "\"Id\":\"1\"" `BSC.isInfixOf` tagJ ]
  check "attachTags: empty tag list attaches nothing; non-object passes through"
    (True, True)
    ( Aeson.encode (attachTags [] tagObj) == Aeson.encode tagObj
    , attachTags ["x"] (Aeson.String "s") == Aeson.String "s" )
  check "attachTags + projectFields: Tags kept when listed, stripped otherwise (case-insensitive)"
    (True, False)
    ( "\"Tags\"" `BSC.isInfixOf` LBS.toStrict (Aeson.encode (projectFields ["tags"] tagV))
    , "\"Tags\"" `BSC.isInfixOf` LBS.toStrict (Aeson.encode (projectFields ["Name"] tagV)) )
  check "applyProviderMerge unions tags (user tags survive provider refresh, case-insensitive dedupe)"
    (["favorite", "HD", "Criterion"], ["favorite", "hd"])
    ( metaTags (applyProviderMerge
      (emptyMetadata { metaTags = ["favorite"], metaLockedFields = ["title"] })
      (emptyMetadata { metaTags = ["HD", "Criterion", "FAVORITE"], metaTitle = Just "T" }))
    , metaTags (applyProviderMerge
      (emptyMetadata { metaTags = ["favorite"] })
      (emptyMetadata { metaTags = ["FAVORITE", "hd"] })) )
  check "tagsFromMetadata counts tags live from metadata records (nub per record, values as stored)"
    [("favorite", 2), ("hd", 1)]
    (tagsFromMetadata [ emptyMetadata { metaTags = ["favorite", "hd", "favorite"] }
                      , emptyMetadata { metaTags = ["favorite"] } ])

  -- Plugin system skeleton: Plugin JSON, overlay/seed merge, hook registry
  let pl = Plugin "livetv" "Live TV" "0.0.0" "Live TV tuners (not implemented)." False
      plJ = LBS.toStrict (Aeson.encode pl)
  check "Plugin ToJSON uses PascalCase keys Id/Name/Version/Description/Enabled"
    [True, True, True, True, True]
    [ "\"Id\":\"livetv\"" `BSC.isInfixOf` plJ
    , "\"Name\":\"Live TV\"" `BSC.isInfixOf` plJ
    , "\"Version\":\"0.0.0\"" `BSC.isInfixOf` plJ
    , "\"Description\":\"Live TV tuners (not implemented).\"" `BSC.isInfixOf` plJ
    , "\"Enabled\":false" `BSC.isInfixOf` plJ ]
  check "Plugin JSON roundtrip"
    (Just pl) (Aeson.decode (Aeson.encode pl) :: Maybe Plugin)
  check "Plugin FromJSON defaults Enabled=True on partial JSON"
    (Just True) (plugEnabled <$> (Aeson.decode "{\"Id\":\"x\"}" :: Maybe Plugin))
  check "builtinPlugins: at least webhooks(enabled), livetv(disabled), dlna(disabled)"
    [("dlna", False), ("livetv", False), ("webhooks", True)]
    (sort [(plugId p, plugEnabled p) | p <- builtinPlugins])
  let persisted = HM.fromList
        [ ("livetv", pl { plugEnabled = True })                       -- disabled builtin, enabled in file
        , ("future-plugin", Plugin "future-plugin" "Future" "9.9" "not built in" True) ]  -- unknown id
      mergedPlugs = overlayPlugins builtinPlugins persisted
  check "overlayPlugins: persisted enabled state wins, unknown persisted id kept, missing builtins defaulted"
    (Just True, Just True, Just False, 4)
    ( plugEnabled <$> HM.lookup "livetv" mergedPlugs
    , plugEnabled <$> HM.lookup "future-plugin" mergedPlugs
    , plugEnabled <$> HM.lookup "dlna" mergedPlugs
    , HM.size mergedPlugs )
  check "registerHook/unregisterHook: dedupe, per-hook lists, removal"
    ( HM.fromList [("scan", ["webhooks", "dlna"]), ("play", ["webhooks"])]
    , HM.fromList [("scan", ["dlna"]), ("play", ["webhooks"])]
    , HM.fromList [("scan", ["webhooks"]), ("play", ["webhooks"])] )
    ( registerHook "scan" "dlna" (registerHook "scan" "webhooks" (registerHook "scan" "webhooks" (registerHook "play" "webhooks" HM.empty)))
    , unregisterHook "scan" "webhooks" (registerHook "scan" "dlna" (registerHook "scan" "webhooks" (registerHook "play" "webhooks" HM.empty)))
    , unregisterHook "scan" "dlna" (registerHook "scan" "dlna" (registerHook "scan" "webhooks" (registerHook "play" "webhooks" HM.empty))) )

  -- TMDB artwork download: pure URL builder + images-JSON mapping
  check "tmdbImagesUrl builds {base}/{kind}/{id}/images?api_key=.. (trailing slash stripped)"
    "http://127.0.0.1:9999/3/movie/550/images?api_key=test"
    (tmdbImagesUrl "http://127.0.0.1:9999/3/" "test" "movie" "550")
  let imgJson = Aeson.decode (LBSC.pack (
        "{\"posters\":[{\"file_path\":\"/p1.jpg\"},{\"file_path\":\"/p2.jpg\"},{\"file_path\":\"\"}],"
        ++ "\"backdrops\":[{\"file_path\":\"/b1.jpg\"},{\"file_path\":\"/b2.jpg\"},{\"file_path\":\"/b3.jpg\"},{\"file_path\":\"/b4.jpg\"}],"
        ++ "\"logos\":[{\"file_path\":\"/l1.png\"},{\"file_path\":\"/l2.png\"}]}"))
  check "tmdbImageListToDownloads: caps (1 Primary, 3 Backdrops, 1 Logo), empty paths skipped"
    (Just [("Primary", "/p1.jpg"), ("Backdrop", "/b1.jpg"), ("Backdrop", "/b2.jpg"), ("Backdrop", "/b3.jpg"), ("Logo", "/l1.png")])
    (tmdbImageListToDownloads <$> imgJson)
  check "tmdbImageListToDownloads: missing keys -> empty; non-object -> empty"
    ([], [], [])
    ( tmdbImageListToDownloads (Aeson.object [])
    , tmdbImageListToDownloads (Aeson.String "x")
    , tmdbImageListToDownloads (fromMaybe Aeson.Null (Aeson.decode (LBSC.pack "{\"posters\":\"nope\"}") :: Maybe Aeson.Value)) )

  putStrLn "All tests passed."
