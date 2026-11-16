module Main (main) where

import Control.Monad (when)
import Data.Acid (closeAcidState, openLocalStateFrom, query, update)
import Data.Acid.Local (createCheckpointAndClose)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, sort)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , findExecutable
  , removeDirectoryRecursive
  , removeFile
  )
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath ((</>))
import System.Process (proc, readCreateProcessWithExitCode)

-- ffmpeg writes arbitrary bytes to stderr; decode pipes as latin1 so the
-- String-based readCreateProcessWithExitCode below can never hit a
-- "cannot decode byte sequence" error regardless of locale.
import GHC.IO.Encoding (latin1, setLocaleEncoding)

import FMS.Core.Module (Segment(..))
import FMS.Core.Query (applyItemQuery, paginate, queryParams, searchItems, sortItems)
import FMS.Core.Range (ByteRange(..), parseRangeHeader)
import FMS.Core.Router (isOpenPath, matchSegments, parseTokenHeader)
import FMS.Core.Types.MediaItem (MediaItem(..))
import FMS.Core.Types.Progress (ItemStats(..), Progress(..))
import FMS.Core.Types.User (User(..), toInternalJSON)
import FMS.Modules.Auth.Monad (hashPassword, verifyPassword)
import FMS.Modules.Auth.State (loadUsers, saveUsers)
import FMS.Modules.Library.Scan (isMediaFile, mimeFor, scanDirectory)
import FMS.Modules.Stats.Monad (applyPlay, applyProgress, continueWatching, popular)
import FMS.Modules.Stats.Persistence (loadStats, saveStats)
import FMS.Modules.Hls (hlsArgs, hlsDirFor)
import FMS.Modules.Stream (resolveRange)
import FMS.Modules.Subtitles (pickSubtitles, subtitleExts, subtitleToVttArgs)
import FMS.Modules.Thumbs (thumbArgs, thumbPathFor)
import FMS.Modules.Transcode (transcodeArgs)
import FMS.Modules.Library.State
  ( GetAllItems(..)
  , GetIdByPath(..)
  , GetItemById(..)
  , RegisterItem(..)
  , initialLibraryState
  )

-- | Plain base-only assertions (no extra test deps): print PASS/FAIL per
-- case and exit non-zero on the first failure.
check :: (Eq a, Show a) => String -> a -> a -> IO ()
check label expected actual
  | expected == actual = putStrLn ("PASS: " ++ label)
  | otherwise = do
      putStrLn ("FAIL: " ++ label
        ++ " — expected " ++ show expected ++ ", got " ++ show actual)
      exitFailure

-- | Remove a scratch dir under /tmp if it exists.
resetDir :: FilePath -> IO ()
resetDir dir = do
  exists <- doesDirectoryExist dir
  if exists then removeDirectoryRecursive dir else pure ()

main :: IO ()
main = do
  setLocaleEncoding latin1
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
  scanned <- scanDirectory scanDir
  check "scan finds only media files"
    (sort [scanDir </> "Movies" </> "a.mp4", scanDir </> "song.mp3"])
    (sort (map itemPath scanned))
  check "scan titles are base names" ["a", "song"] (sort (map (T.unpack . itemTitle) scanned))
  check "scan sizes" [11, 11] (sort (map itemSize scanned))
  check "scan mimes" ["audio/mpeg", "video/mp4"] (sort (map (T.unpack . itemMime) scanned))
  check "scan assigns empty id (RegisterItem fills it)" ["", ""] (sort (map (T.unpack . itemId) scanned))
  check "mimeFor .mp4" "video/mp4" (mimeFor (scanDir </> "Movies" </> "a.mp4"))
  check "mimeFor .mp3" "audio/mpeg" (mimeFor (scanDir </> "song.mp3"))
  check "isMediaFile .txt is False" False (isMediaFile (scanDir </> "notes.txt"))
  check "isMediaFile .MKV (case-insensitive) is True" True (isMediaFile "/x/Y.MKV")
  missing <- scanDirectory "/tmp/fms-test-scan-does-not-exist"
  check "scan of missing dir is []" [] missing
  removeDirectoryRecursive scanDir

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
  closeAcidState st2
  removeDirectoryRecursive acidDir

  -- Transcode module (milestone 3b): the pure ffmpeg argument list.
  let targs = transcodeArgs "/tmp/fms-test-tc.wav"
  check "transcodeArgs contains -f mp4" True ("-f" `elem` targs && "mp4" `elem` targs)
  check "transcodeArgs contains pipe:1" True ("pipe:1" `elem` targs)
  check "transcodeArgs contains the input path" True ("/tmp/fms-test-tc.wav" `elem` targs)

  -- IO test of the actual transcode, guarded by ffmpeg availability: skip
  -- gracefully when ffmpeg is not on PATH.
  mFFmpeg <- findExecutable "ffmpeg"
  case mFFmpeg of
    Nothing -> putStrLn "SKIP: ffmpeg not found on PATH — skipping transcode IO test"
    Just _ -> do
      let wav = "/tmp/fms-test-tc.wav"
      (ecGen, _, genErr) <- readCreateProcessWithExitCode
        (proc "ffmpeg" ["-f", "lavfi", "-i", "sine=frequency=440:duration=1", "-y", wav]) ""
      check "generate 1s test wav: exit Success" ExitSuccess ecGen
      when (ecGen /= ExitSuccess) (putStrLn ("ffmpeg wav gen stderr: " ++ show genErr))
      (ec, out, tcErr) <- readCreateProcessWithExitCode
        (proc "ffmpeg" (transcodeArgs wav)) ""
      check "transcodeArgs run on wav: exit Success" ExitSuccess ec
      when (ec /= ExitSuccess) (putStrLn ("ffmpeg transcode stderr: " ++ show tcErr))
      check "transcoded output is > 1000 bytes" True (length out > 1000)
      -- An MP4 starts with a size box whose type at offset 4 is "ftyp"
      -- (0x66 0x74 0x79 0x70 = [102,116,121,112]).
      check "output bytes 4..8 are the ftyp box type"
        [0x66, 0x74, 0x79, 0x70] (map fromEnum (take 4 (drop 4 out)))

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
    Nothing -> putStrLn "SKIP: ffmpeg not found on PATH — skipping HLS IO test"
    Just _ -> do
      let mp4    = "/tmp/fms-test-hls-src.mp4"
          outDir = "/tmp/fms-test-hls-out"
          m3u8   = outDir </> "main.m3u8"
      resetDir outDir
      createDirectoryIfMissing True outDir
      (ecGen, _, genErr) <- readCreateProcessWithExitCode
        (proc "ffmpeg"
          [ "-f", "lavfi", "-i", "testsrc=duration=4:size=160x120:rate=10"
          , "-c:v", "libx264", "-y", mp4 ]) ""
      check "generate 4s test mp4: exit Success" ExitSuccess ecGen
      when (ecGen /= ExitSuccess) (putStrLn ("ffmpeg mp4 gen stderr: " ++ show genErr))
      (ec, _, hlsErr) <- readCreateProcessWithExitCode
        (proc "ffmpeg" (hlsArgs mp4 m3u8)) ""
      check "hlsArgs run on 4s mp4: exit Success" ExitSuccess ec
      when (ec /= ExitSuccess) (putStrLn ("ffmpeg hls stderr: " ++ show hlsErr))
      m3u8Exists <- doesFileExist m3u8
      check "main.m3u8 exists after ffmpeg" True m3u8Exists
      playlist <- readFile m3u8
      check "playlist starts with #EXTM3U" True ("#EXTM3U" `isPrefixOf` playlist)
      let segNames = filter (".ts" `isSuffixOf`) (lines playlist)
      check "playlist references at least one .ts segment" True (not (null segNames))
      segsExist <- mapM (\s -> doesFileExist (outDir </> s)) segNames
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
    Nothing -> putStrLn "SKIP: ffmpeg not found on PATH — skipping subtitle IO test"
    Just _ -> do
      let srt = "/tmp/fms-sub-test.srt"
      writeFile srt "1\n00:00:01,000 --> 00:00:02,000\nHello\n"
      (ec, out, subErr) <- readCreateProcessWithExitCode
        (proc "ffmpeg" (subtitleToVttArgs srt)) ""
      check "subtitleToVttArgs run on srt: exit Success" ExitSuccess ec
      when (ec /= ExitSuccess) (putStrLn ("ffmpeg subtitle stderr: " ++ show subErr))
      check "converted output contains WEBVTT" True ("WEBVTT" `isInfixOf` out)
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
    Nothing -> putStrLn "SKIP: ffmpeg not found on PATH — skipping thumbs IO test"
    Just _ -> do
      let mp4 = "/tmp/fms-test-thumb-src.mp4"
      (ecGen, _, genErr) <- readCreateProcessWithExitCode
        (proc "ffmpeg"
          [ "-f", "lavfi", "-i", "testsrc=duration=6:size=160x120:rate=10"
          , "-c:v", "libx264", "-y", mp4 ]) ""
      check "generate 6s test mp4: exit Success" ExitSuccess ecGen
      when (ecGen /= ExitSuccess) (putStrLn ("ffmpeg mp4 gen stderr: " ++ show genErr))
      (ec, out, thErr) <- readCreateProcessWithExitCode
        (proc "ffmpeg" (thumbArgs mp4)) ""
      check "thumbArgs run on 6s mp4: exit Success" ExitSuccess ec
      when (ec /= ExitSuccess) (putStrLn ("ffmpeg thumb stderr: " ++ show thErr))
      -- A JPEG starts with the SOI marker 0xFF 0xD8.
      check "thumb output starts with JPEG SOI (FFD8)"
        [0xFF, 0xD8] (map fromEnum (take 2 out))
      check "thumb output is > 1000 bytes" True (length out > 1000)
      removeFile mp4

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
      u1 = User "1" "admin" "$2y$14$somehash" True ["key1"]
      u2 = User "2" "bob" "hash2" False []
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
      uj3 = LBS.toStrict (Aeson.encode (User "3" "nopw" "" False []))
  check "User ToJSON omits passwordHash" False ("passwordHash" `BSC.isInfixOf` uj1)
  check "User ToJSON has hasPassword:true when a hash is set"
    True ("\"hasPassword\":true" `BSC.isInfixOf` uj1)
  check "User ToJSON has hasPassword:false when the hash is empty"
    True ("\"hasPassword\":false" `BSC.isInfixOf` uj3)
  check "User ToJSON carries id/name/isAdmin"
    True (all (`BSC.isInfixOf` uj1) ["\"id\":\"1\"", "\"name\":\"admin\"", "\"isAdmin\":true"])
  check "User FromJSON parses the internal full shape"
    (Just u1) (Aeson.decode (Aeson.encode (toInternalJSON u1)) :: Maybe User)

  putStrLn "All tests passed."
