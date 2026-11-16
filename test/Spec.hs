module Main (main) where

import Control.Monad (when)
import Data.Acid (closeAcidState, openLocalStateFrom, query, update)
import Data.Acid.Local (createCheckpointAndClose)
import Data.List (sort)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import System.Directory
  ( createDirectoryIfMissing
  , doesDirectoryExist
  , findExecutable
  , removeDirectoryRecursive
  )
import System.Exit (ExitCode(..), exitFailure)
import System.FilePath ((</>))
import System.Process (proc, readCreateProcessWithExitCode)

-- ffmpeg writes arbitrary bytes to stderr; decode pipes as latin1 so the
-- String-based readCreateProcessWithExitCode below can never hit a
-- "cannot decode byte sequence" error regardless of locale.
import GHC.IO.Encoding (latin1, setLocaleEncoding)

import FMS.Core.Module (Segment(..))
import FMS.Core.Range (ByteRange(..), parseRangeHeader)
import FMS.Core.Router (matchSegments)
import FMS.Core.Types.MediaItem (MediaItem(..))
import FMS.Modules.Library.Scan (isMediaFile, mimeFor, scanDirectory)
import FMS.Modules.Stream (resolveRange)
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

  putStrLn "All tests passed."
