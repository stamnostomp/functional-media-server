module FMS.Modules.Playback.Types
  ( MediaStream(..)
  , MediaSource(..)
  , PlaybackResponse(..)
  ) where

import Data.Aeson (ToJSON(..), object, (.=))
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
-- sources plus a fresh play session id.
data PlaybackResponse = PlaybackResponse
  { mediaSources  :: ![MediaSource]
  , playSessionId :: !Text
  } deriving (Show, Eq)

instance ToJSON PlaybackResponse where
  toJSON r = object
    [ "MediaSources"  .= mediaSources r
    , "PlaySessionId" .= playSessionId r
    ]
