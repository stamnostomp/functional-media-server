module FMS.Modules.Segments.Routes
  ( segmentsRoutes
  ) where

import Control.Monad (forM_)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (FromJSON (..), eitherDecode, object, withObject, (.:), (.:?), (.!=), (.=))
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Network.Wai (Request, Response, strictRequestBody)
import System.Exit (ExitCode (..))

import FMS.Core.Module (Route, Segment (..), delete, get, jsonResponse, post)
import FMS.Core.Monad (AppError (..), AppM, acquireFfmpegSlot, envConfig, releaseFfmpegSlot)
import FMS.Core.Process (runProcessBSTimeout)
import FMS.Core.SceneDetect (blackdetectArgs, intervalsOverlap, parseBlackdetect, parseSilencedetect, proposeSegments, silencedetectArgs)
import FMS.Core.Types.Config (Config (..))
import FMS.Core.Types.MediaItem (MediaItem (..))
import FMS.Modules.Auth.Monad (getItemWithAccess, requireAdmin)
import FMS.Modules.Playback.Probe (probeDuration)
import FMS.Modules.Segments.Monad (MonadSegments (..), newSegmentId)
import FMS.Modules.Segments.Types (MediaSegment (..), secondsToTicks, ticksToSeconds, validateSegment)

-- | The media-segments API (docs/todo.org section 6, milestones 6h +
-- auto-detection): @GET/POST /items/:id/MediaSegments@,
-- @POST /items/:id/MediaSegments/AutoDetect@ and @DELETE
-- /items/:id/MediaSegments/:segId@. Segments are normally stored
-- manually; AutoDetect derives Intro/Outro markers from ffmpeg
-- blackdetect/silencedetect (admin only). All routes enforce
-- folder/policy access on the item via 'getItemWithAccess'; bad bodies
-- or invalid segments are 400, unknown items/segments 404.
segmentsRoutes :: [Route]
segmentsRoutes =
  [ get    [Lit "items", Capture "id", Lit "MediaSegments"] listSegmentsHandler
  , post   [Lit "items", Capture "id", Lit "MediaSegments"] createSegmentHandler
  , post   [Lit "items", Capture "id", Lit "MediaSegments", Lit "AutoDetect"] autoDetectHandler
  , delete [Lit "items", Capture "id", Lit "MediaSegments", Capture "segId"] deleteSegmentHandler
    -- Jellyfin-compat aliases (todo): /Videos/:id/MediaSegments and
    -- /Audio/:id/MediaSegments are the same route group (the handlers
    -- only read the "id"/"segId" captures). segmentsModule is ordered
    -- BEFORE hlsModule in FMS.Server so these literals win over HLS's
    -- [videos, :id, Capture seg].
  , get    [Lit "Videos", Capture "id", Lit "MediaSegments"] listSegmentsHandler
  , post   [Lit "Videos", Capture "id", Lit "MediaSegments"] createSegmentHandler
  , post   [Lit "Videos", Capture "id", Lit "MediaSegments", Lit "AutoDetect"] autoDetectHandler
  , delete [Lit "Videos", Capture "id", Lit "MediaSegments", Capture "segId"] deleteSegmentHandler
  , get    [Lit "Audio", Capture "id", Lit "MediaSegments"] listSegmentsHandler
  , post   [Lit "Audio", Capture "id", Lit "MediaSegments"] createSegmentHandler
  , post   [Lit "Audio", Capture "id", Lit "MediaSegments", Lit "AutoDetect"] autoDetectHandler
  , delete [Lit "Audio", Capture "id", Lit "MediaSegments", Capture "segId"] deleteSegmentHandler
  ]

-- | Body of @POST /items/:id/MediaSegments@:
-- @{\"Type\": \"Intro\", \"StartTicks\": n, \"EndTicks\": m, \"Id\"?: \"...\"}@.
data CreateSegmentRequest = CreateSegmentRequest
  { csrId         :: Maybe Text
  , csrType       :: Text
  , csrStartTicks :: Integer
  , csrEndTicks   :: Integer
  }

instance FromJSON CreateSegmentRequest where
  parseJSON = withObject "CreateSegmentRequest" $ \o ->
    CreateSegmentRequest
      <$> o .:? "Id"
      <*> o .:  "Type"
      <*> o .:? "StartTicks" .!= 0
      <*> o .:? "EndTicks" .!= 0

-- Handlers ------------------------------------------------------------------

-- | @GET \/items\/:id\/MediaSegments@ — 404 unknown item, otherwise 200
-- with the item's segment array (empty when none).
listSegmentsHandler :: [(Text, Text)] -> Request -> AppM Response
listSegmentsHandler captures req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just i  -> do
      mItem <- getItemWithAccess req i
      case mItem of
        Nothing -> throwError (NotFound ("no library item with id " <> i))
        Just _  -> jsonResponse <$> listSegments i

-- | @POST \/items\/:id\/MediaSegments@ — validate the body ('validateSegment',
-- 400 on invalid), mint a 16-hex id when absent, upsert, 200 the segment.
createSegmentHandler :: [(Text, Text)] -> Request -> AppM Response
createSegmentHandler captures req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just i  -> do
      mItem <- getItemWithAccess req i
      case mItem of
        Nothing -> throwError (NotFound ("no library item with id " <> i))
        Just _ -> do
          body <- liftIO (strictRequestBody req)
          csr <- case eitherDecode body of
            Left err -> throwError (BadRequest ("invalid JSON body: " <> T.pack err))
            Right v  -> pure v
          sid <- case csrId csr of
            Just s | not (T.null s) -> pure s
            _                       -> liftIO newSegmentId
          let seg = MediaSegment sid i (csrType csr) (csrStartTicks csr) (csrEndTicks csr)
          case validateSegment seg of
            Left e   -> throwError (BadRequest e)
            Right s  -> jsonResponse <$> upsertSegment s

-- | @POST \/items\/:id\/MediaSegments\/AutoDetect@ (admin only) — run
-- ffmpeg blackdetect + silencedetect on the item's video file, propose
-- Intro/Outro ranges with 'proposeSegments' and upsert them (skipping
-- ranges that already have a segment of that type). Responds 200 with
-- the item's full segment list after the operation. 400 when the item
-- is not a video, the duration is unknown, or ffmpeg fails; 404 for an
-- unknown item.
autoDetectHandler :: [(Text, Text)] -> Request -> AppM Response
autoDetectHandler captures req = do
  _ <- requireAdmin req
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just i  -> do
      mItem <- getItemWithAccess req i
      case mItem of
        Nothing -> throwError (NotFound ("no library item with id " <> i))
        Just item -> do
          let path = case itemVersions item of
                (v : _) -> v
                []      -> itemPath item
          if not ("video" `T.isPrefixOf` itemMime item)
            then throwError (BadRequest "AutoDetect requires a video item")
            else do
              mDur <- liftIO (probeDuration path)
              case mDur of
                Nothing -> throwError (BadRequest "could not determine the item duration")
                Just dur -> do
                  cfg <- asks envConfig
                  mSem <- acquireFfmpegSlot
                  (blacks, blackOk) <- liftIO (runDetectPass cfg (blackdetectArgs path) parseBlackdetect)
                  (sils, silOk)     <- liftIO (runDetectPass cfg (silencedetectArgs path) parseSilencedetect)
                  liftIO (releaseFfmpegSlot mSem)
                  if not (blackOk && silOk)
                    then throwError (BadRequest "ffmpeg scene detection failed")
                    else do
                      let (mIntro, mOutro) = proposeSegments dur blacks sils
                      existing <- listSegments i
                      let overlapsExisting ty (a, b) =
                            any (\s -> segType s == ty
                                       && intervalsOverlap
                                            (ticksToSeconds (segStartTicks s), ticksToSeconds (segEndTicks s))
                                            (a, b))
                                existing
                      forM_ [("Intro", mIntro), ("Outro", mOutro)] $ \(ty, mRange) ->
                        case mRange of
                          Just (a, b) | b > a, not (overlapsExisting ty (a, b)) -> do
                            sid <- liftIO newSegmentId
                            let seg = MediaSegment sid i ty (secondsToTicks a) (secondsToTicks b)
                            case validateSegment seg of
                              Left e  -> throwError (BadRequest e)
                              Right s -> do
                                _ <- upsertSegment s
                                pure ()
                          _ -> pure ()
                      jsonResponse <$> listSegments i

-- | One ffmpeg detection pass: run @ffmpeg args@ with the configured
-- timeout, parse the filter output on stderr, and report whether the
-- exit code was success.
runDetectPass
  :: Config
  -> [String]
  -> (String -> [(Double, Double)])
  -> IO ([(Double, Double)], Bool)
runDetectPass cfg args parse = do
  (ec, _out, err) <- runProcessBSTimeout (configFfmpegTimeoutSec cfg) "ffmpeg" args
  pure (parse (BSC.unpack (LBS.toStrict err)), ec == ExitSuccess)

-- | @DELETE \/items\/:id\/MediaSegments\/:segId@ — 200 @{\"status\":\"ok\"}@
-- or 404 when the item/segment does not exist.
deleteSegmentHandler :: [(Text, Text)] -> Request -> AppM Response
deleteSegmentHandler captures req =
  case (lookup "id" captures, lookup "segId" captures) of
    (Nothing, _) -> throwError (BadRequest "missing id capture")
    (_, Nothing) -> throwError (BadRequest "missing segId capture")
    (Just i, Just sid) -> do
      mItem <- getItemWithAccess req i
      case mItem of
        Nothing -> throwError (NotFound ("no library item with id " <> i))
        Just _  -> do
          ok <- deleteSegment i sid
          if ok
            then pure (jsonResponse (object ["status" .= ("ok" :: Text)]))
            else throwError (NotFound ("no segment " <> sid <> " for item " <> i))
