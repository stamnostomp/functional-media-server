module Main (main) where

import Control.Monad (when)
import Data.Acid (closeAcidState, openLocalStateFrom, query, update)
import Data.Acid.Local (createCheckpointAndClose)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.HashMap.Strict as HM
import Data.List (isInfixOf, isPrefixOf, sort)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
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

import FMS.Core.Module (Segment(..))
import FMS.Core.Process (runProcessBS, showBS)
import FMS.Core.Query (applyItemQuery, paginate, queryParams, searchItems, sortItems)
import FMS.Core.Range (ByteRange(..), parseRangeHeader)
import FMS.Core.Router (isOpenPath, matchSegments, parseTokenHeader)
import FMS.Core.Types.MediaItem (MediaItem(..))
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
import FMS.Modules.Auth.Monad (hashPassword, verifyPassword)
import FMS.Modules.Auth.State (loadUsers, saveUsers)
import FMS.Modules.Library.Scan (isMediaFile, mimeFor, scanDirectory)
import FMS.Modules.Stats.Monad (applyPlay, applyProgress, continueWatching, popular)
import FMS.Modules.Stats.Persistence (loadStats, saveStats)
import FMS.Modules.UserData.Monad (markPlayed, setFavorite, setPosition)
import FMS.Modules.UserData.Persistence (loadUserData, saveUserData)
import FMS.Modules.UserData.Types (UserData (..), emptyUserData)
import FMS.Modules.Hls (hlsArgs, hlsDirFor)
import FMS.Modules.Images (imageExtFor, imagePathFor, isValidImageType, resizeArgs)
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
import FMS.Modules.Metadata.Api.Tmdb (parseTmdbResults, tmdbSearchUrl)
import FMS.Modules.Metadata.Api.Types (TmdbResult (..))
import FMS.Modules.Metadata.Nfo (nfoPathFor, parseMovieNfo)
import FMS.Modules.Metadata.Persistence (loadMetadata, saveMetadata)
import FMS.Modules.Metadata.Routes (genresFromMetadata)
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
        (Just "Only Title") Nothing [] Nothing Nothing Nothing HM.empty "nfo"
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

  putStrLn "All tests passed."
