module FMS.Modules.Transcode
  ( transcodeModule
  , transcodeArgs
  ) where

import Control.Monad.Except (throwError)
import Data.ByteString.Builder (Builder, byteString)
import Data.Text (Text)
import Network.HTTP.Types (status200)
import Network.Wai (Request, Response, responseStream)

import FMS.Core.Module (Module(..), Segment(..), get)
import FMS.Core.Monad (AppError(..), AppM)
import FMS.Core.Process (runProcessStreamBS)
import FMS.Core.Types.MediaItem (MediaItem(..))
import FMS.Modules.Auth.Monad (getItemWithAccess)

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
transcodeHandler captures req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just i  -> do
      mItem <- getItemWithAccess req i
      case mItem of
        Nothing -> throwError (NotFound ("no library item with id " <> i))
        Just item ->
          pure (responseStream status200
                  [("Content-Type", "video/mp4")]
                  (streamTranscode (itemPath item)))

-- | The 'Network.Wai.StreamingBody' used with 'responseStream': run
-- ffmpeg on @path@ and write its stdout to the WAI sink in chunks of
-- 64 KiB, like 'FMS.Modules.Subtitles.streamToVtt'.
-- 'runProcessStreamBS' streams stdout chunk-by-chunk and drains stderr as
-- raw bytes concurrently (never locale-decoded), and it always closes
-- the pipes and reaps ffmpeg — even if the client disconnects mid-stream
-- and the sink throws.
streamTranscode
  :: FilePath
  -> (Builder -> IO ()) -> IO () -> IO ()
streamTranscode path send flush = do
  _ <- runProcessStreamBS "ffmpeg" (transcodeArgs path) (send . byteString)
  flush
