module FMS.Modules.Subtitles
  ( subtitlesModule
  , subtitleExts
  , pickSubtitles
  , subtitleToVttArgs
  , subtitleList
  ) where

import Control.Exception (SomeException, bracket, try)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Data.Char (toLower)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder (Builder, byteString)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types (status200)
import Network.Wai (Request, Response, responseLBS, responseStream)
import System.Directory (listDirectory)
import System.Exit (ExitCode)
import System.FilePath ((</>), takeBaseName, takeDirectory, takeExtension)
import System.IO (Handle, hClose)
import System.Process
  ( CreateProcess(..)
  , ProcessHandle
  , StdStream(..)
  , createProcess
  , proc
  , waitForProcess
  )

import FMS.Core.Module (Module(..), Segment(..), get, jsonResponse)
import FMS.Core.Monad (AppError(..), AppM)
import FMS.Core.Types.MediaItem (MediaItem(..))
import FMS.Modules.Library (MonadLibrary(..))

-- | The subtitles module (docs/roadmap.org milestone 3d, docs/todo.org
-- section 7): detect external subtitle files sitting next to a media
-- item, list them, and serve them as WebVTT — raw when the file already
-- is @.vtt@, otherwise converted on the fly by ffmpeg. Routes:
--
-- * @GET /items/:id/subtitles@ — JSON array of subtitle entries
--   @[{\"index\":0,\"file\":\"/abs/clip.srt\",\"format\":\"srt\"}, ...]@
-- * @GET /stream/:id/subtitle/:index@ — that subtitle as @text/vtt@
--
-- Like the stream module it only needs the 'MonadLibrary' capability —
-- the core never learns about it (docs/modules.org). ffmpeg is found on
-- PATH at runtime; the Nix package wraps the @fms@ binary with ffmpeg on
-- its PATH (flake.nix).
subtitlesModule :: Module
subtitlesModule = Module
  { moduleName   = "subtitles"
  , moduleInit   = pure ()
  , moduleRoutes =
      [ get [Lit "items", Capture "id", Lit "subtitles"] listSubtitlesHandler
      , get [Lit "stream", Capture "id", Lit "subtitle", Capture "index"]
          serveSubtitleHandler
      ]
  }

-- | Recognised external subtitle extensions, lowercase (compared
-- case-insensitively in 'pickSubtitles').
subtitleExts :: [String]
subtitleExts = [".srt", ".vtt", ".ass", ".ssa", ".sub"]

-- | Given the media file path and the entries of its directory, return
-- the matching subtitle file paths (absolute, joined onto the media's
-- directory), always sorted so the indexes are stable. Files whose
-- basename matches the media basename (@takeBaseName media <ext>@) are
-- preferred; when none match, every subtitle file in the directory is
-- returned. Pure so it is unit-testable.
pickSubtitles :: FilePath -> [FilePath] -> [FilePath]
pickSubtitles media entries =
  let dir  = takeDirectory media
      base = map toLower (takeBaseName media)
      isSub e = map toLower (takeExtension e) `elem` subtitleExts
      subs = filter isSub entries
      matching = filter (\e -> map toLower (takeBaseName e) == base) subs
      chosen = if null matching then subs else matching
  in sort (map (dir </>) chosen)

-- | The ffmpeg argument list converting the subtitle file at @path@ to
-- WebVTT on stdout. Pure so it is unit-testable.
subtitleToVttArgs :: FilePath -> [String]
subtitleToVttArgs path =
  [ "-v", "error"
  , "-i", path
  , "-f", "webvtt"
  , "pipe:1"
  ]

-- | IO wrapper around 'pickSubtitles': list the directory of @media@,
-- pick its subtitles, and zip them with 0-based indexes and the format
-- (the lowercase extension without the dot).
subtitleList :: FilePath -> IO [(Int, FilePath, String)]
subtitleList media = do
  entries <- listDirectory (takeDirectory media)
  let subs = pickSubtitles media entries
  pure [ (i, p, formatOf p) | (i, p) <- zip [0 ..] subs ]
  where
    formatOf p = drop 1 (map toLower (takeExtension p))

-- | @GET \/items\/:id\/subtitles@ — 404 for an unknown item, otherwise a
-- JSON array of @{\"index\",\"file\",\"format\"}@ entries for the external
-- subtitle files found next to the item's media file.
listSubtitlesHandler :: [(Text, Text)] -> Request -> AppM Response
listSubtitlesHandler captures _req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just i  -> do
      mItem <- getItem i
      case mItem of
        Nothing -> throwError (NotFound ("no library item with id " <> i))
        Just item -> do
          subs <- liftIO (subtitleList (itemPath item))
          pure (jsonResponse
            [ object
                [ "index"  .= n
                , "file"   .= p
                , "format" .= fmt
                ]
            | (n, p, fmt) <- subs
            ])

-- | @GET \/stream\/:id\/subtitle\/:index@ — serve one external subtitle
-- of the item as @text/vtt@. Unknown item is 404; an @index@ that is not
-- an integer or is out of range is 400. A @.vtt@ file is served raw;
-- anything else is converted to WebVTT by ffmpeg and streamed.
serveSubtitleHandler :: [(Text, Text)] -> Request -> AppM Response
serveSubtitleHandler captures _req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just i  -> case lookup "index" captures of
      Nothing -> throwError (BadRequest "missing index capture")
      Just idxTxt ->
        case reads (T.unpack idxTxt) :: [(Int, String)] of
          [(n, "")] | n >= 0 -> do
            mItem <- getItem i
            case mItem of
              Nothing -> throwError (NotFound ("no library item with id " <> i))
              Just item -> do
                subs <- liftIO (subtitleList (itemPath item))
                case [ p | (n', p, _) <- subs, n' == n ] of
                  [] -> throwError (BadRequest
                    ("subtitle index " <> idxTxt <> " out of range ("
                     <> T.pack (show (length subs)) <> " subtitle(s) for item " <> i <> ")"))
                  (path : _) ->
                    if map toLower (takeExtension path) == ".vtt"
                      then do
                        bs <- liftIO (LBS.readFile path)
                        pure (responseLBS status200 [("Content-Type", "text/vtt")] bs)
                      else
                        pure (responseStream status200
                                [("Content-Type", "text/vtt")]
                                (streamToVtt path))
          _ -> throwError (BadRequest ("invalid subtitle index: " <> idxTxt))

-- | The 'Network.Wai.StreamingBody' used with 'responseStream': run
-- ffmpeg converting @path@ to WebVTT and write its stdout to the WAI sink
-- in chunks of 64 KiB, like 'FMS.Modules.Transcode.streamTranscode'. The
-- process is bracketed so ffmpeg is always reaped and its pipe handles
-- closed, even if the client disconnects mid-stream.
streamToVtt
  :: FilePath
  -> (Builder -> IO ()) -> IO () -> IO ()
streamToVtt path send flush =
  bracket startFFmpeg stopFFmpeg $ \(ph, out, err) -> do
    let go = do
          chunk <- BS.hGet out chunkSize
          if BS.null chunk
            then pure ()  -- EOF: ffmpeg finished (or died); stop cleanly
            else do
              send (byteString chunk)
              go
    go
    hClose err
    _ <- waitForProcess ph
    flush
  where
    startFFmpeg = do
      r <- createProcess (proc "ffmpeg" (subtitleToVttArgs path))
            { std_out = CreatePipe, std_err = CreatePipe }
      case r of
        (_, Just out, Just err, ph) -> pure (ph, out, err)
        _ -> fail "ffmpeg pipes were not created"

    stopFFmpeg :: (ProcessHandle, Handle, Handle) -> IO ()
    stopFFmpeg (ph, out, err) = do
      _ <- try (hClose out) :: IO (Either SomeException ())
      _ <- try (hClose err) :: IO (Either SomeException ())
      _ <- try (waitForProcess ph) :: IO (Either SomeException ExitCode)
      pure ()

-- | Streaming chunk size: 64 KiB.
chunkSize :: Int
chunkSize = 64 * 1024
