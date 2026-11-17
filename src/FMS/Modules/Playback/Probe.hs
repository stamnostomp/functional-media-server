module FMS.Modules.Playback.Probe
  ( probeArgs
  , parseFfprobe
  , probeFile
  , probeBitrateArgs
  , parseFormatBitRate
  , probeBitrate
  , parseFormatDuration
  , probeDuration
  , decidePlayMethod
  , containerFromFormatName
  , containerFromExtension
  , directPlayable
  , directStreamable
  , transcoding
  , directPlayContainers
  , directStreamContainers
  , directPlayVideoCodecs
  , directPlayAudioCodecs
  ) where

import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON(..), Value(..), withObject, (.:), (.:?), (.!=))
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Exit (ExitCode(..))
import System.FilePath (takeExtension)
import Text.Read (readMaybe)

import FMS.Core.Process (runProcessBS, showBS)
import FMS.Modules.Playback.Types (MediaStream(..))

-- | The ffprobe argument list for inspecting @path@: quiet, JSON on
-- stdout, both the format and the stream sections. Pure so it is
-- unit-testable (same pattern as 'FMS.Modules.Hls.hlsArgs').
probeArgs :: FilePath -> [String]
probeArgs path =
  [ "-v", "error"
  , "-print_format", "json"
  , "-show_format"
  , "-show_streams"
  , path
  ]

-- | Run ffprobe on @path@ and parse its JSON output. A non-zero exit
-- (unreadable / unprobeable file) comes back as 'Left' with the stderr
-- tail rendered byte-safely via 'showBS' (never locale-decoded, so it
-- cannot throw regardless of the process locale); a zero exit with
-- unparseable output is also 'Left'.
probeFile :: FilePath -> IO (Either String (Text, [MediaStream]))
probeFile path = do
  (exitCode, out, err) <- runProcessBS "ffprobe" (probeArgs path)
  case exitCode of
    ExitSuccess -> pure (parseFfprobe out)
    ExitFailure n ->
      pure (Left ("ffprobe exited with code " ++ show n ++ ": "
                  ++ take 300 (showBS (LBS.toStrict err))))

-- | Pure parser for ffprobe's @-print_format json -show_format
-- -show_streams@ output: @(container, streams)@. The container is the
-- first token of @format.format_name@ (the mov/mp4/m4a family is
-- reported canonically as "mp4", matroska as "mkv"); each entry of
-- @streams[]@ becomes a 'MediaStream'. Malformed JSON, or JSON whose
-- shape is wrong, is a 'Left'.
parseFfprobe :: LBS.ByteString -> Either String (Text, [MediaStream])
parseFfprobe body = do
  doc <- Aeson.eitherDecode body :: Either String ProbeDoc
  let mFormat   = pdFormat doc
      container = containerFromFormatName (pfName =<< mFormat)
      fmtDur    = mFormat >>= pfDuration
      streams   = map (toMediaStream fmtDur) (pdStreams doc)
  pure (container, streams)

-- | The ffprobe argument list reading only the format section of
-- @path@ (used to learn the file's bit_rate for the milestone 6f
-- bitrate caps). Pure so it is unit-testable.
probeBitrateArgs :: FilePath -> [String]
probeBitrateArgs path =
  [ "-v", "error"
  , "-print_format", "json"
  , "-show_format"
  , path
  ]

-- | Pure parser for ffprobe's @-show_format@ JSON: the format
-- @bit_rate@ in bps, when present and parseable.
parseFormatBitRate :: LBS.ByteString -> Maybe Int
parseFormatBitRate body =
  case Aeson.eitherDecode body :: Either String ProbeFormatDoc of
    Right doc -> pfBitRate (pfFormat doc)
    Left _    -> Nothing

-- | Run ffprobe with 'probeBitrateArgs' and return the format bit_rate
-- (bps), or 'Nothing' on any failure (unreadable file, bad output).
probeBitrate :: FilePath -> IO (Maybe Int)
probeBitrate path = do
  (exitCode, out, _err) <- runProcessBS "ffprobe" (probeBitrateArgs path)
  case exitCode of
    ExitSuccess -> pure (parseFormatBitRate out)
    ExitFailure _ -> pure Nothing

-- | Pure parser for ffprobe's @-show_format@ JSON: the format
-- @duration@ in seconds, when present. Same output shape as
-- 'parseFormatBitRate'.
parseFormatDuration :: LBS.ByteString -> Maybe Double
parseFormatDuration body =
  case Aeson.eitherDecode body :: Either String ProbeFormatDoc of
    Right doc -> pfDuration (pfFormat doc)
    Left _    -> Nothing

-- | Run ffprobe with 'probeBitrateArgs' and return the format duration
-- in seconds, or 'Nothing' on any failure.
probeDuration :: FilePath -> IO (Maybe Double)
probeDuration path = do
  (exitCode, out, _err) <- runProcessBS "ffprobe" (probeBitrateArgs path)
  case exitCode of
    ExitSuccess -> pure (parseFormatDuration out)
    ExitFailure _ -> pure Nothing

-- Internal shape for 'parseFormatBitRate'.
data ProbeFormatDoc = ProbeFormatDoc { pfFormat :: ProbeFormat }

instance FromJSON ProbeFormatDoc where
  parseJSON = withObject "ffprobe format output" $ \o ->
    ProbeFormatDoc <$> o .: "format"

-- | The playback decision (milestone 6f): "DirectPlay" when the source
-- is direct playable AND no effective bitrate cap excludes it — an
-- unknown source bitrate passes (documented), a known one must be
-- <= the cap. Otherwise "Transcode".
decidePlayMethod :: Bool -> Maybe Int -> Maybe Int -> Text
decidePlayMethod directPlayable' mSourceBitrate mCap
  | directPlayable' && bitrateOk = "DirectPlay"
  | otherwise                    = "Transcode"
  where
    bitrateOk = case mCap of
      Nothing -> True
      Just c  -> maybe True (<= c) mSourceBitrate

-- | The container name from ffprobe's comma-separated @format_name@
-- (e.g. @\"mov,mp4,m4a,3gp,3g2,mj2\"@). The mov/mp4 family shares one
-- demuxer, so when "mp4" is among the tokens the canonical name "mp4"
-- is reported (this is what Jellyfin does); otherwise the first token
-- wins (@\"matroska,webm\"@ normalises to "mkv").
containerFromFormatName :: Maybe Text -> Text
containerFromFormatName mName =
  case filter (not . T.null) (map T.strip (T.splitOn "," (fromMaybe "" mName))) of
    []         -> ""
    (t : rest)
      | "mp4" `elem` (t : rest) -> "mp4"
      | t == "matroska"         -> "mkv"
      | otherwise               -> t

-- | A fallback container guess from the file extension, used when
-- ffprobe cannot read the file at all (e.g. a dummy/corrupt file):
-- @/a/clip.MP4@ -> "mp4", no extension -> "unknown".
containerFromExtension :: FilePath -> Text
containerFromExtension path =
  case takeExtension path of
    []   -> "unknown"
    ext  -> T.pack (map toLower (drop 1 ext))

-- Internal aeson shapes for the ffprobe document. Durations are emitted
-- by ffprobe as strings ("2.024000") but numbers are tolerated too.
data ProbeDoc = ProbeDoc
  { pdFormat  :: Maybe ProbeFormat
  , pdStreams :: [ProbeStream]
  }

instance FromJSON ProbeDoc where
  parseJSON = withObject "ffprobe output" $ \o ->
    ProbeDoc
      <$> o .:? "format"
      <*> o .:? "streams" .!= []

data ProbeFormat = ProbeFormat
  { pfName     :: Maybe Text
  , pfDuration :: Maybe Double
  , pfBitRate  :: Maybe Int    -- ^ milestone 6f: format bit_rate in bps
  }

instance FromJSON ProbeFormat where
  parseJSON = withObject "format" $ \o -> do
    name <- o .:? "format_name"
    dur  <- o .:? "duration"
    br   <- o .:? "bit_rate"
    pure (ProbeFormat name (dur >>= unFlexSeconds) (br >>= unFlexInt))

data ProbeStream = ProbeStream
  { psIndex     :: !Int
  , psCodecName :: Maybe Text
  , psCodecType :: Maybe Text
  , psTags      :: Maybe ProbeTags
  , psWidth     :: Maybe Int
  , psHeight    :: Maybe Int
  , psChannels  :: Maybe Int
  , psDuration  :: Maybe Double
  }

instance FromJSON ProbeStream where
  parseJSON = withObject "stream" $ \o -> do
    idx  <- o .: "index"
    cn   <- o .:? "codec_name"
    ct   <- o .:? "codec_type"
    tags <- o .:? "tags"
    w    <- o .:? "width"
    h    <- o .:? "height"
    ch   <- o .:? "channels"
    dur  <- o .:? "duration"
    pure (ProbeStream idx cn ct tags w h ch (dur >>= unFlexSeconds))

data ProbeTags = ProbeTags
  { ptLanguage :: Maybe Text
  , ptTitle    :: Maybe Text
  }

instance FromJSON ProbeTags where
  parseJSON = withObject "tags" $ \o ->
    ProbeTags <$> o .:? "language" <*> o .:? "title"

-- | ffprobe emits durations as JSON strings; accept numbers too.
newtype FlexSeconds = FlexSeconds (Maybe Double)

unFlexSeconds :: FlexSeconds -> Maybe Double
unFlexSeconds (FlexSeconds d) = d

instance FromJSON FlexSeconds where
  parseJSON (String t) = pure (FlexSeconds (readMaybe (T.unpack t)))
  parseJSON (Number n) = pure (FlexSeconds (readMaybe (show n)))
  parseJSON _          = pure (FlexSeconds Nothing)

-- | ffprobe emits bit_rate as a JSON string ("91767"); accept numbers too.
newtype FlexInt = FlexInt (Maybe Int)

unFlexInt :: FlexInt -> Maybe Int
unFlexInt (FlexInt d) = d

instance FromJSON FlexInt where
  parseJSON (String t) = pure (FlexInt (readMaybe (T.unpack t)))
  parseJSON (Number n) = pure (FlexInt (Just (floor (realToFrac n :: Double))))
  parseJSON _          = pure (FlexInt Nothing)

-- | Map one parsed ffprobe stream to a 'MediaStream'. The duration is
-- the stream's own when present, otherwise the format's.
toMediaStream :: Maybe Double -> ProbeStream -> MediaStream
toMediaStream fmtDur ps =
  MediaStream
    { msIndex           = psIndex ps
    , msType            = streamTypeName (psCodecType ps)
    , msCodec           = fromMaybe "" (psCodecName ps)
    , msLanguage        = ptLanguage =<< psTags ps
    , msTitle           = ptTitle =<< psTags ps
    , msWidth           = psWidth ps
    , msHeight          = psHeight ps
    , msChannels        = psChannels ps
    , msDurationSeconds = case psDuration ps of
        Just d  -> Just d
        Nothing -> fmtDur
    , msIsDefault       = False
    }

-- | @video@/@audio@/@subtitle@ -> "Video"/"Audio"/"Subtitle"; anything
-- else is capitalised as-is ("data" -> "Data").
streamTypeName :: Maybe Text -> Text
streamTypeName (Just t) = case T.toLower (T.strip t) of
  "video"    -> "Video"
  "audio"    -> "Audio"
  "subtitle" -> "Subtitle"
  other      -> T.toUpper (T.take 1 other) <> T.drop 1 other
streamTypeName Nothing = "Unknown"

-- The pure playback-decision logic (docs/todo.org section 6,
-- milestone 6a), exported for unit tests. The default client profile
-- is assumed: containers and codecs a browser/Jellyfin client can play
-- natively.

-- | Containers that can be sent to the client as-is (direct play).
directPlayContainers :: [Text]
directPlayContainers =
  ["mp4", "m4v", "mov", "webm", "mp3", "m4a", "flac", "wav", "ogg"]

-- | Containers that must be remuxed into a streamable container
-- (direct stream) but whose codecs need no re-encode.
directStreamContainers :: [Text]
directStreamContainers = ["mkv", "avi", "ts", "m2ts", "wmv"]

-- | Video codecs the default profile can decode natively.
directPlayVideoCodecs :: [Text]
directPlayVideoCodecs =
  ["h264", "hevc", "vp8", "vp9", "av1", "mpeg4", "mpeg2video"]

-- | Audio codecs the default profile can decode natively.
directPlayAudioCodecs :: [Text]
directPlayAudioCodecs =
  ["aac", "mp3", "opus", "flac", "vorbis", "pcm_s16le"]

-- | Can the file be sent to the client without any change? The
-- container must be directly playable AND every video stream's codec
-- must be in 'directPlayVideoCodecs' AND every audio stream's codec in
-- 'directPlayAudioCodecs'. A file with no video/audio streams at all
-- (e.g. audio-only) is direct playable when its container is.
directPlayable :: Text -> [MediaStream] -> Bool
directPlayable c streams =
  c `elem` directPlayContainers
    && all (\s -> msCodec s `elem` directPlayVideoCodecs) (filter isVideo streams)
    && all (\s -> msCodec s `elem` directPlayAudioCodecs) (filter isAudio streams)
  where
    isVideo s = msType s == "Video"
    isAudio s = msType s == "Audio"

-- | Can the file be remuxed (container change only, no re-encode)?
-- Purely container-driven for the default profile.
directStreamable :: Text -> [MediaStream] -> Bool
directStreamable c _streams = c `elem` directStreamContainers

-- | Does playback require a full transcode? By definition here: the
-- file is not direct playable.
transcoding :: Text -> [MediaStream] -> Bool
transcoding c streams = not (directPlayable c streams)
