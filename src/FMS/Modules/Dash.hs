module FMS.Modules.Dash
  ( dashModule
  , dashArgs
  , dashArgsWith
  , dashDirFor
  , ensureDash
  , isValidDashSegName
  ) where

import Control.Exception (bracket_)
import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Char (isAlphaNum)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types (status200)
import Network.Wai (Request, Response, queryString, responseLBS)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import Text.Read (readMaybe)

import FMS.Core.Module (Module(..), Segment(..), get)
import FMS.Core.Monad (AppError(..), AppM, MonadLog(..), acquireFfmpegSlot, envConfig, releaseFfmpegSlot)
import FMS.Core.Process (runProcessBSTimeout, showBS)
import FMS.Core.Query (queryParams, versionParam, versionPathFor)
import FMS.Core.Types.Config (Config(..))
import FMS.Core.Types.MediaItem (MediaItem(..))
import FMS.Core.Types.Policy (UserPolicy(..))
import FMS.Core.Types.User (User(..))
import FMS.Modules.Auth.Monad (currentUserFromReq, getItemWithAccess)
import FMS.Modules.Transcode (effectiveBitrateCap)

-- | The DASH module (docs/todo.org section 6, milestone 3g): Jellyfin-style
-- MPEG-DASH packaging, @GET /videos/:id/dash.mpd@ plus the segment files
-- @GET /videos/:id/dash/:seg@. Segments are produced once per item
-- (per version) by ffmpeg into @stateDir/dash/<id>-<version>/@ and cached
-- there; the manifest route lazily triggers packaging on first request
-- ('ensureDash'). The module must be registered BEFORE the HLS module so
-- that @videos/:id/dash.mpd@ is not captured by the HLS 3-segment route.
-- @?version=N@ picks the media version (milestone 4j) and the effective
-- bitrate cap (milestone 6f) is honoured. Subprocess output is captured
-- as raw bytes via "FMS.Core.Process" — never locale-decoded.
dashModule :: Module
dashModule = Module
  { moduleName   = "dash"
  , moduleInit   = pure ()
  , moduleRoutes =
      -- Registered before the HLS module in src/FMS/Server.hs so that
      -- videos/:id/dash.mpd is not captured by the HLS [videos, id, seg]
      -- route ("dash.mpd" would otherwise pass isValidSegName).
      [ get [Lit "videos", Capture "id", Lit "dash.mpd"] manifestHandler
      , get [Lit "videos", Capture "id", Lit "dash", Capture "seg"] dashSegmentHandler
      ]
  }

-- | The ffmpeg argument list for DASH VOD packaging of @input@ into
-- @outputMpd@ with no bitrate cap. Pure so it is unit-testable: re-encode
-- video to H.264 and audio to AAC, segment on 4-second boundaries with a
-- template-based (SegmentTemplate) MPD, and write the init segments as
-- @init-stream$Number$.m4s@ and the media segments as
-- @chunk-stream$Number$-$Number$.m4s@ next to the manifest.
dashArgs :: FilePath -> FilePath -> [String]
dashArgs = dashArgsWith Nothing

-- | Like 'dashArgs' but with an optional bitrate cap in bps, which adds
-- @-b:v \/-maxrate \/-bufsize@ (same shape as
-- 'FMS.Modules.Transcode.transcodeArgsWith').
dashArgsWith :: Maybe Int -> FilePath -> FilePath -> [String]
dashArgsWith mBitrate input outputMpd =
  [ "-v", "error"
  , "-i", input
  , "-map", "0:v"
  , "-map", "0:a?"
  , "-c:v", "libx264"
  , "-preset", "veryfast"
  , "-pix_fmt", "yuv420p"
  ]
  ++ dashBitrateArgs mBitrate
  ++ [ "-c:a", "aac"
     , "-f", "dash"
     , "-seg_duration", "4"
     , "-use_template", "1"
     , "-use_timeline", "0"
     , "-init_seg_name", "init-stream$Number$.m4s"
     , "-media_seg_name", "chunk-stream$Number$-$Number$.m4s"
     , outputMpd
     ]

-- | The @-b:v \/-maxrate \/-bufsize@ triple for a bitrate cap (same
-- convention as the transcode module: bufsize = 2x the bitrate).
dashBitrateArgs :: Maybe Int -> [String]
dashBitrateArgs (Just bps) | bps > 0 =
  [ "-b:v", show bps, "-maxrate", show bps, "-bufsize", show (2 * bps) ]
dashBitrateArgs _ = []

-- | The per-item-per-version DASH cache directory:
-- @stateDir \/ \"dash\" \/ \<id\>-\<version\>@.
dashDirFor :: Config -> Text -> Int -> FilePath
dashDirFor cfg i version =
  configStateDir cfg </> "dash" </> (T.unpack i ++ "-" ++ show version)

-- | Make sure the DASH assets for @item@ at version @version@ exist under
-- the state dir and return the manifest path. If @manifest.mpd@ is already
-- there the cached copy is reused; otherwise ffmpeg runs synchronously.
-- A non-zero ffmpeg exit is logged (ASCII, byte-safe stderr tail) but does
-- NOT throw — the manifest path is still returned and the manifest route
-- answers 404 when the file is absent (graceful degradation, same policy
-- as PlaybackInfo on unprobeable files). @mCap@ is the effective bitrate
-- cap (milestone 6f) passed through to 'dashArgsWith'.
ensureDash :: Config -> MediaItem -> Int -> Maybe Int -> AppM FilePath
ensureDash cfg item version mCap = do
  let dir   = dashDirFor cfg (itemId item) version
      mpd   = dir </> "manifest.mpd"
      input = case versionPathFor item version of
        Right p -> p
        Left _  -> itemPath item   -- unreachable: callers validate first
  exists <- liftIO (doesFileExist mpd)
  unless exists $ do
    liftIO (createDirectoryIfMissing True dir)
    logInfo ("dash: packaging item " <> itemId item <> " version "
             <> T.pack (show version) <> " into " <> T.pack dir)
    -- milestone 15: run packaging under the ffmpeg slot semaphore (503 on
    -- exhaustion) with the wall-clock killer timeout; the slot is released
    -- when the job ends either way
    mRelease <- acquireFfmpegSlot
    r <- liftIO $ bracket_ (pure ()) (releaseFfmpegSlot mRelease)
           (runProcessBSTimeout (configFfmpegTimeoutSec cfg)
              "ffmpeg" (dashArgsWith mCap input mpd))
    case r of
      (ExitSuccess, _out, _err)   -> pure ()
      (ExitFailure n, _out, err) ->
        logErr ("dash: ffmpeg packaging failed (exit " <> T.pack (show n) <> "): "
                <> T.pack (take 2000 (showBS (LBS.toStrict err))))
  pure mpd

-- | @GET \/videos\/:id\/dash.mpd@ — 404 for an unknown item, 400 for a bad
-- @version@, otherwise ensure the DASH assets exist and serve the manifest
-- as @application\/dash+xml@. ffmpeg writes relative segment names, so
-- serving the manifest from this path prefix makes the segment URLs work.
manifestHandler :: [(Text, Text)] -> Request -> AppM Response
manifestHandler captures req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just i  -> do
      mItem <- getItemWithAccess req i
      case mItem of
        Nothing -> throwError (NotFound ("no library item with id " <> i))
        Just item -> do
          version <- selectedVersion item req
          cfg <- asks envConfig
          mUser <- currentUserFromReq req
          let qparams = queryParams (queryString req)
              policyCap = mUser >>= policyMaxStreamingBitrate . userPolicy
              clientCap = lookup "maxbitrate" qparams >>= \v -> readMaybe (T.unpack v)
              cap = effectiveBitrateCap policyCap clientCap (configMaxBitrate cfg)
          mpd <- ensureDash cfg item version cap
          ok <- liftIO (doesFileExist mpd)
          if ok
            then serveFile mpd "application/dash+xml"
            else throwError (NotFound ("no dash manifest for item " <> i))

-- | @GET \/videos\/:id\/dash\/:seg@ — serve one cached DASH segment from
-- @stateDir\/dash\/<id>-<version>\/<seg>@. Init segments (@init-*@) are
-- @video/mp4@, media segments @video/iso.segment@. Unknown item or missing
-- segment is 404; a segment name outside @[A-Za-z0-9._-]+@ (or containing
-- @..@) is rejected with 400 so it can never traverse out of the dash dir.
dashSegmentHandler :: [(Text, Text)] -> Request -> AppM Response
dashSegmentHandler captures req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just i  -> case lookup "seg" captures of
      Nothing -> throwError (BadRequest "missing seg capture")
      Just seg
        | not (isValidDashSegName seg) ->
            throwError (BadRequest ("invalid segment name: " <> seg))
        | otherwise -> do
            mItem <- getItemWithAccess req i
            case mItem of
              Nothing -> throwError (NotFound ("no library item with id " <> i))
              Just _item -> do
                version <- case versionParam (queryParams (queryString req)) of
                  Left e  -> throwError (BadRequest e)
                  Right n -> pure n
                cfg <- asks envConfig
                let segPath = dashDirFor cfg i version </> T.unpack seg
                    ctype = if "init-" `T.isPrefixOf` seg
                              then "video/mp4"
                              else "video/iso.segment"
                ok <- liftIO (doesFileExist segPath)
                if ok
                  then serveFile segPath ctype
                  else throwError (NotFound ("no dash segment " <> seg <> " for item " <> i))

-- | Resolve the @version@ query param against the item and validate it
-- against the version list: any problem is a 400 before packaging starts.
selectedVersion :: MediaItem -> Request -> AppM Int
selectedVersion item req =
  case versionParam (queryParams (queryString req)) of
    Left e  -> throwError (BadRequest e)
    Right n -> case versionPathFor item n of
      Left e -> throwError (BadRequest e)
      Right _ -> pure n

-- | A segment name is safe to join onto the dash dir only if it is
-- non-empty, consists solely of @[A-Za-z0-9._-]@, and contains no @..@
-- (same rule as the HLS module's 'isValidSegName').
isValidDashSegName :: Text -> Bool
isValidDashSegName s =
  not (T.null s)
  && T.all (\c -> isAlphaNum c || c == '.' || c == '_' || c == '-') s
  && not (".." `T.isInfixOf` s)

-- | Read a whole file and serve it with 'responseLBS' and the given
-- Content-Type — simple and safe for manifest/segment sizes.
serveFile :: FilePath -> BS.ByteString -> AppM Response
serveFile path contentType = do
  bs <- liftIO (LBS.readFile path)
  pure (responseLBS status200 [("Content-Type", contentType)] bs)
