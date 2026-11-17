module FMS.Modules.Transcode
  ( transcodeModule
  , transcodeArgs
  , transcodeArgsWith
  , effectiveBitrateCap
  , hwEncoderFor
  , normalizeHwAccel  -- re-exported from FMS.Core.Types.Config
  ) where

import Control.Concurrent.QSemN (QSemN)
import Control.Exception (bracket_)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (asks)
import Data.ByteString.Builder (Builder, byteString)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types (status200)
import Network.Wai (Request, Response, queryString, responseStream)
import Text.Read (readMaybe)

import FMS.Core.Module (Module(..), Segment(..), get)
import FMS.Core.Monad (AppError(..), AppM, acquireFfmpegSlot, envConfig, releaseFfmpegSlot)
import FMS.Core.Process (ffmpegTimeoutArgs, runProcessStreamBSTimeout)
import FMS.Core.Query (queryParams, versionParam, versionPathFor)
import FMS.Core.Types.Config (Config(..), normalizeHwAccel)
import FMS.Core.Types.Policy (UserPolicy(..))
import FMS.Core.Types.User (User(..))
import FMS.Modules.Auth.Monad (currentUserFromReq, getItemWithAccess)

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
-- Equivalent to 'transcodeArgsWith' with no hardware acceleration and no
-- bitrate cap.
transcodeArgs :: FilePath -> [String]
transcodeArgs = transcodeArgsWith "none" Nothing

-- | The H.264 encoder matching a hardware-acceleration name
-- (milestone 6f): vaapi -> h264_vaapi, nvenc -> h264_nvenc, qsv ->
-- h264_qsv, videotoolbox -> h264_videotoolbox; anything else -> 'Nothing'
-- (fall back to libx264). Pure.
hwEncoderFor :: Text -> Maybe Text
hwEncoderFor accel = case normalizeHwAccel accel of
  "vaapi"        -> Just "h264_vaapi"
  "nvenc"        -> Just "h264_nvenc"
  "qsv"          -> Just "h264_qsv"
  "videotoolbox" -> Just "h264_videotoolbox"
  _              -> Nothing

-- | The effective bitrate cap in bps (milestone 6f): the minimum of the
-- caps that are set (> 0) among the user-policy cap, the client-requested
-- cap and the server-wide cap; 'Nothing' when none is set. Pure.
effectiveBitrateCap :: Maybe Int -> Maybe Int -> Int -> Maybe Int
effectiveBitrateCap mUserCap mClientCap serverCap =
  case filter (> 0) (catMaybes [mUserCap, mClientCap, serverCapNonZero]) of
    [] -> Nothing
    cs -> Just (minimum cs)
  where
    serverCapNonZero = if serverCap > 0 then Just serverCap else Nothing

-- | The ffmpeg argument list for a transcode of @path@ with hardware
-- acceleration @hwaccel@ (see 'normalizeHwAccel') and an optional
-- bitrate cap @maxBitrate@ in bps. Pure so it is unit-testable:
-- @-hwaccel <accel>@ goes before @-i@ when enabled; the encoder comes
-- from 'hwEncoderFor' (libx264 fallback, and the x264-only @-preset@
-- is only added for libx264); a set cap adds @-b:v/-maxrate/-bufsize@.
transcodeArgsWith :: Text -> Maybe Int -> FilePath -> [String]
transcodeArgsWith hwaccel mBitrate path =
  [ "-v", "error" ]
  ++ hwaccelArgs
  ++ [ "-i", path
     , "-c:v", encoder
     ]
  ++ presetArgs
  ++ [ "-pix_fmt", "yuv420p" ]
  ++ bitrateArgs
  ++ [ "-c:a", "aac"
     , "-movflags", "frag_keyframe+empty_moov"
     , "-f", "mp4"
     , "pipe:1"
     ]
  where
    accel  = normalizeHwAccel hwaccel
    hwaccelArgs = if accel == "none" then [] else ["-hwaccel", T.unpack accel]
    encoder = T.unpack (fromMaybe "libx264" (hwEncoderFor accel))
    presetArgs = if encoder == "libx264" then ["-preset", "veryfast"] else []
    bitrateArgs = case mBitrate of
      Just bps | bps > 0 ->
        [ "-b:v", show bps, "-maxrate", show bps, "-bufsize", show (2 * bps) ]
      _ -> []

-- | @GET /stream/:id/transcode@ — 404 for a missing item, otherwise 200
-- @video/mp4@ streamed from ffmpeg's stdout. No Content-Length: the
-- length is unknown until ffmpeg finishes, so warp uses chunked
-- transfer encoding. Milestone 6f: the effective bitrate cap is the min
-- of the requesting user's policy cap, the @?maxBitrate=@ query param
-- (invalid values are ignored, documented) and the server-wide
-- FMS_MAX_BITRATE; the encoder/hwaccel comes from FMS_HWACCEL.
transcodeHandler :: [(Text, Text)] -> Request -> AppM Response
transcodeHandler captures req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just i  -> do
      mItem <- getItemWithAccess req i
      case mItem of
        Nothing -> throwError (NotFound ("no library item with id " <> i))
        Just item -> do
          -- milestone 4j: ?version=N selects the version file to transcode
          path <- case versionParam (queryParams (queryString req)) of
            Left msg -> throwError (BadRequest msg)
            Right n  -> case versionPathFor item n of
              Left msg   -> throwError (BadRequest msg)
              Right path -> pure path
          cfg <- asks envConfig
          mUser <- currentUserFromReq req
          let qparams = queryParams (queryString req)
              policyCap = mUser >>= policyMaxStreamingBitrate . userPolicy
              -- invalid maxBitrate is ignored (documented), not a 400
              clientCap = lookup "maxbitrate" qparams >>= \v -> readMaybe (T.unpack v)
              cap = effectiveBitrateCap policyCap clientCap (configMaxBitrate cfg)
              -- milestone 15: ffmpeg's own read timeout for network inputs
              -- (before -i), on top of the killer-thread guard below
              args = ffmpegTimeoutArgs (configFfmpegTimeoutSec cfg) path
                     ++ transcodeArgsWith (configHwAccel cfg) cap path
          -- milestone 15: take an ffmpeg slot (5s grace) or answer 503
          mRelease <- acquireFfmpegSlot
          pure (responseStream status200
                  [("Content-Type", "video/mp4")]
                  (streamTranscode (configFfmpegTimeoutSec cfg) mRelease args))

-- | The 'Network.Wai.StreamingBody' used with 'responseStream': run
-- ffmpeg with @args@ and write its stdout to the WAI sink in chunks of
-- 64 KiB, like 'FMS.Modules.Subtitles.streamToVtt'.
-- 'runProcessStreamBSTimeout' streams stdout chunk-by-chunk and drains
-- stderr as raw bytes concurrently (never locale-decoded), and it always
-- closes the pipes and reaps ffmpeg — even if the client disconnects
-- mid-stream and the sink throws. Milestone 15: @timeoutSec@ bounds the
-- whole ffmpeg job (killer thread, <= 0 disables) and @mRelease@ frees
-- the ffmpeg slot taken by 'acquireFfmpegSlot' when streaming ends for
-- any reason (bracket_ releases it even on a client disconnect).
streamTranscode
  :: Int
  -> Maybe QSemN
  -> [String]
  -> (Builder -> IO ()) -> IO () -> IO ()
streamTranscode timeoutSec mRelease args send flush =
  bracket_ (pure ()) (releaseFfmpegSlot mRelease) $ do
    _ <- runProcessStreamBSTimeout timeoutSec "ffmpeg" args (send . byteString)
    flush
