module FMS.Modules.Transcode
  ( transcodeModule
  , transcodeArgs
  ) where

import Control.Exception (SomeException, bracket, try)
import Control.Monad.Except (throwError)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder, byteString)
import Data.Text (Text)
import Network.HTTP.Types (status200)
import Network.Wai (Request, Response, responseStream)
import System.Exit (ExitCode)
import System.IO (Handle, hClose)
import System.Process
  ( CreateProcess(..)
  , ProcessHandle
  , StdStream(..)
  , createProcess
  , proc
  , waitForProcess
  )

import FMS.Core.Module (Module(..), Segment(..), get)
import FMS.Core.Monad (AppError(..), AppM)
import FMS.Core.Types.MediaItem (MediaItem(..))
import FMS.Modules.Library (MonadLibrary(..))

-- | The transcode module (docs/roadmap.org milestone 3b, docs/todo.org
-- section 6): @GET /stream/:id/transcode@ pipes the library item through
-- ffmpeg and streams the resulting fragmented MP4 to the client, so any
-- input ffmpeg can read is playable as @video/mp4@. Like the stream
-- module it only needs the 'MonadLibrary' capability — the core never
-- learns about it (docs/modules.org). ffmpeg is found on PATH at runtime;
-- the Nix package wraps the @fms@ binary with ffmpeg on its PATH
-- (flake.nix).
transcodeModule :: Module
transcodeModule = Module
  { moduleName   = "transcode"
  , moduleInit   = pure ()
  , moduleRoutes = [get [Lit "stream", Capture "id", Lit "transcode"] transcodeHandler]
  }

-- | The ffmpeg argument list for an on-the-fly transcode of @path@ to a
-- fragmented MP4 on stdout. Pure so it is unit-testable: re-encode video
-- to H.264 (veryfast, yuv420p for broad player support) and audio to AAC,
-- and write a fragmented MP4 (@frag_keyframe+empty_moov@) to @pipe:1@ so
-- the output can be streamed while it is still being produced.
transcodeArgs :: FilePath -> [String]
transcodeArgs path =
  [ "-v", "error"
  , "-i", path
  , "-c:v", "libx264"
  , "-preset", "veryfast"
  , "-pix_fmt", "yuv420p"
  , "-c:a", "aac"
  , "-movflags", "frag_keyframe+empty_moov"
  , "-f", "mp4"
  , "pipe:1"
  ]

-- | @GET /stream/:id/transcode@ — 404 for a missing item, otherwise 200
-- @video/mp4@ streamed from ffmpeg's stdout. No Content-Length: the
-- length is unknown until ffmpeg finishes, so warp uses chunked
-- transfer encoding.
transcodeHandler :: [(Text, Text)] -> Request -> AppM Response
transcodeHandler captures _req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just i  -> do
      mItem <- getItem i
      case mItem of
        Nothing -> throwError (NotFound ("no library item with id " <> i))
        Just item ->
          pure (responseStream status200
                  [("Content-Type", "video/mp4")]
                  (streamTranscode (itemPath item)))

-- | The 'Network.Wai.StreamingBody' used with 'responseStream': run
-- ffmpeg on @path@ and write its stdout to the WAI sink in chunks of
-- 64 KiB, like 'FMS.Modules.Stream.streamFileRange'. The process is
-- bracketed so ffmpeg is always reaped and its pipe handles closed, even
-- if the client disconnects mid-stream. When stdout hits EOF, stderr is
-- closed and the process waited on; the bracket release then only has
-- to tolerate the already-done cleanup.
streamTranscode
  :: FilePath
  -> (Builder -> IO ()) -> IO () -> IO ()
streamTranscode path send flush =
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
      r <- createProcess (proc "ffmpeg" (transcodeArgs path))
            { std_out = CreatePipe, std_err = CreatePipe }
      case r of
        -- createProcess returns (stdin, stdout, stderr, ph); only stdout
        -- and stderr are pipes here.
        (_, Just out, Just err, ph) -> pure (ph, out, err)
        _ -> fail "ffmpeg pipes were not created"

    -- Always reap ffmpeg and close the pipes; each step is tolerant of
    -- the body having already closed/closed-and-waited them.
    stopFFmpeg :: (ProcessHandle, Handle, Handle) -> IO ()
    stopFFmpeg (ph, out, err) = do
      _ <- try (hClose out) :: IO (Either SomeException ())
      _ <- try (hClose err) :: IO (Either SomeException ())
      _ <- try (waitForProcess ph) :: IO (Either SomeException ExitCode)
      pure ()

-- | Streaming chunk size: 64 KiB.
chunkSize :: Int
chunkSize = 64 * 1024
