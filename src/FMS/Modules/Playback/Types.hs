module FMS.Modules.Playback.Types
  ( MediaStream(..)
  , MediaSource(..)
  , PlaybackResponse(..)
  , defaultDeviceProfile
  , selectStreams
  , markDefaultStreams
  ) where

import Data.Aeson (ToJSON(..), Value, object, (.=))
import Data.List (find)
import Data.Maybe (listToMaybe)
import Data.Text (Text)

-- | One ffprobe stream of a media file, in the Jellyfin 'MediaStream'
-- shape (docs/todo.org section 6, milestone 6a). 'msType' is one of
-- "Video" / "Audio" / "Subtitle". Optional fields are 'Nothing' when
-- ffprobe did not report them; they encode as JSON null.
data MediaStream = MediaStream
  { msIndex           :: !Int
  , msType            :: !Text             -- ^ "Video" | "Audio" | "Subtitle"
  , msCodec           :: !Text
  , msLanguage        :: !(Maybe Text)
  , msTitle           :: !(Maybe Text)
  , msWidth           :: !(Maybe Int)
  , msHeight          :: !(Maybe Int)
  , msChannels        :: !(Maybe Int)
  , msDurationSeconds :: !(Maybe Double)
  , msIsDefault       :: !Bool             -- ^ set by 'markDefaultStreams' (milestone 6i)
  } deriving (Show, Eq)

instance ToJSON MediaStream where
  toJSON s = object
    [ "Index"           .= msIndex s
    , "Type"            .= msType s
    , "Codec"           .= msCodec s
    , "Language"        .= msLanguage s
    , "Title"           .= msTitle s
    , "Width"           .= msWidth s
    , "Height"          .= msHeight s
    , "Channels"        .= msChannels s
    , "DurationSeconds" .= msDurationSeconds s
    , "IsDefault"       .= msIsDefault s
    ]

-- | One playable source of an item (usually the single file), together
-- with the playback decision flags computed from the probed streams
-- (see "FMS.Modules.Playback.Probe").
data MediaSource = MediaSource
  { sourceId                  :: !Text
  , sourcePath                :: !FilePath
  , container                 :: !Text
  , sizeBytes                 :: !Integer
  , mediaStreams              :: ![MediaStream]
  , supportsDirectPlay        :: !Bool
  , supportsDirectStream      :: !Bool
  , supportsTranscoding       :: !Bool
  } deriving (Show, Eq)

instance ToJSON MediaSource where
  toJSON ms = object
    [ "Id"                    .= sourceId ms
    , "Path"                  .= sourcePath ms
    , "Container"             .= container ms
    , "Size"                  .= sizeBytes ms
    , "MediaStreams"          .= mediaStreams ms
    , "SupportsDirectPlay"    .= supportsDirectPlay ms
    , "SupportsDirectStream"  .= supportsDirectStream ms
    , "SupportsTranscoding"   .= supportsTranscoding ms
    ]

-- | The body of @POST /Items/{id}/PlaybackInfo@: the candidate media
-- sources plus a fresh play session id, and (milestone 6f) the playback
-- decision: @PlayMethod@ ("DirectPlay"|"Transcode"), the effective
-- @MaxStreamingBitrate@ cap when one is set, a @TranscodingUrl@ when
-- transcoding, and the echoed audio/subtitle stream indexes when the
-- client sent them.
data PlaybackResponse = PlaybackResponse
  { mediaSources        :: ![MediaSource]
  , playSessionId       :: !Text
  , playMethod          :: !Text
  , maxStreamingBitrate :: !(Maybe Int)
  , transcodingUrl      :: !(Maybe Text)
  , audioStreamIndex    :: !(Maybe Int)
  , subtitleStreamIndex :: !(Maybe Int)
  } deriving (Show, Eq)

instance ToJSON PlaybackResponse where
  toJSON r = object $
    [ "MediaSources"  .= mediaSources r
    , "PlaySessionId" .= playSessionId r
    , "PlayMethod"    .= playMethod r
    ] ++ [ "MaxStreamingBitrate" .= b | Just b <- [maxStreamingBitrate r] ]
      ++ [ "TranscodingUrl" .= u | Just u <- [transcodingUrl r] ]
      ++ [ "AudioStreamIndex" .= a | Just a <- [audioStreamIndex r] ]
      ++ [ "SubtitleStreamIndex" .= s | Just s <- [subtitleStreamIndex r] ]

-- The static device profile + stream-selection logic (milestone 6i,
-- docs/todo.org section 6), all pure so it is unit-testable. The profile
-- mirrors the container/codec lists the playback decision in
-- "FMS.Modules.Playback.Probe" assumes for the default client.

-- | The static default device profile served by @GET /DeviceProfiles@:
-- one Jellyfin-shaped profile ("Default") whose direct-play
-- containers/codecs match 'directPlayContainers' /
-- 'directPlayVideoCodecs' / 'directPlayAudioCodecs' in
-- "FMS.Modules.Playback.Probe" (plus the direct-stream containers in
-- the video entry), and whose transcoding profile matches what
-- "FMS.Modules.Transcode" produces (mp4 + h264 + aac).
defaultDeviceProfile :: Value
defaultDeviceProfile = object
  [ "Name" .= ("Default" :: Text)
  , "DirectPlayProfiles" .= directPlayProfiles
  , "TranscodingProfiles" .= transcodeProfiles
  , "SubtitleProfiles" .= ([] :: [Value])
  ]
  where
    directPlayProfiles =
      [ object
          [ "Type" .= ("Video" :: Text)
          , "Container" .= ("mp4,m4v,mov,webm,mkv,avi,ts,m2ts,wmv,mp3,m4a,flac,wav,ogg" :: Text)
          , "VideoCodec" .= ("h264,hevc,vp8,vp9,av1,mpeg4,mpeg2video" :: Text)
          , "AudioCodec" .= ("aac,mp3,opus,flac,vorbis,pcm_s16le" :: Text)
          ]
      , object
          [ "Type" .= ("Audio" :: Text)
          , "Container" .= ("mp3,m4a,flac,wav,ogg,webm" :: Text)
          , "AudioCodec" .= ("aac,mp3,opus,flac,vorbis" :: Text)
          ]
      , object
          [ "Type" .= ("Subtitle" :: Text)
          , "Container" .= ("srt,ass,ssa,vtt,sub" :: Text)
          ]
      ]
    transcodeProfiles =
      [ object
          [ "Type" .= ("Video" :: Text)
          , "Container" .= ("mp4" :: Text)
          , "VideoCodec" .= ("h264" :: Text)
          , "AudioCodec" .= ("aac" :: Text)
          ]
      ]

-- | Pick the effective audio and subtitle streams for a PlaybackInfo
-- request (milestone 6i). Audio: matched by 'msIndex' when an index is
-- given (a negative index means "none", the Jellyfin convention),
-- falling back to the first audio stream when the index is absent or
-- matches nothing. Subtitle: matched by 'msIndex' only, with NO
-- fallback -- an absent or unmatched index means no subtitle.
selectStreams
  :: Maybe Int            -- ^ requested AudioStreamIndex
  -> Maybe Int            -- ^ requested SubtitleStreamIndex
  -> [MediaStream]
  -> (Maybe MediaStream, Maybe MediaStream)
selectStreams mAudio mSub streams =
  ( pickAudio mAudio auds
  , pickSub   mSub   subs
  )
  where
    auds = [ s | s <- streams, msType s == "Audio" ]
    subs = [ s | s <- streams, msType s == "Subtitle" ]
    pickAudio (Just n) xs
      | n < 0     = Nothing
      | otherwise = case find ((== n) . msIndex) xs of
          Just s  -> Just s
          Nothing -> listToMaybe xs
    pickAudio Nothing xs = listToMaybe xs
    pickSub (Just n) xs
      | n < 0     = Nothing
      | otherwise = find ((== n) . msIndex) xs
    pickSub Nothing _ = Nothing

-- | Mark the chosen audio/subtitle streams as default in the list
-- (milestone 6i): every stream whose 'msIndex' equals the chosen
-- stream's index gets 'msIsDefault' = True, all others False. Video
-- streams are never marked by this helper.
markDefaultStreams
  :: Maybe MediaStream    -- ^ chosen audio stream
  -> Maybe MediaStream    -- ^ chosen subtitle stream
  -> [MediaStream]
  -> [MediaStream]
markDefaultStreams mAudio mSub = map mark
  where
    aIdx = msIndex <$> mAudio
    sIdx = msIndex <$> mSub
    mark s = s { msIsDefault = Just (msIndex s) == aIdx || Just (msIndex s) == sIdx }
