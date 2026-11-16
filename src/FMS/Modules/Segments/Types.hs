module FMS.Modules.Segments.Types
  ( MediaSegment(..)
  , segmentTypes
  , validSegmentType
  , ticksToSeconds
  , secondsToTicks
  , validateSegment
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:?), (.!=), (.=))
import Data.Text (Text)
import qualified Data.Text as T

-- | One media segment marker (docs/todo.org section 6, milestone 6h):
-- an intro/outro/preview/... skip range on an item, persisted in
-- @stateDir/mediaSegments.json@. Auto-detection is OUT OF SCOPE —
-- segments are stored manually via the API. Ticks are 100 ns units
-- (the Jellyfin convention).
data MediaSegment = MediaSegment
  { segId         :: !Text     -- ^ minted server-side when absent (16 hex)
  , segItemId     :: !Text     -- ^ the library item this segment belongs to
  , segType       :: !Text     -- ^ "Intro"|"Outro"|"Preview"|"Recap"|"Commercial"
  , segStartTicks :: !Integer
  , segEndTicks   :: !Integer
  } deriving (Eq, Show)

-- | API JSON with Jellyfin-style PascalCase keys.
instance ToJSON MediaSegment where
  toJSON s = object
    [ "Id"         .= segId s
    , "ItemId"     .= segItemId s
    , "Type"       .= segType s
    , "StartTicks" .= segStartTicks s
    , "EndTicks"   .= segEndTicks s
    ]

-- | Missing @Id@ defaults to empty (the routes mint one); the rest are
-- required but tolerate defaults so hand-edited files still load.
instance FromJSON MediaSegment where
  parseJSON = withObject "MediaSegment" $ \o ->
    MediaSegment
      <$> o .:? "Id" .!= ""
      <*> o .:? "ItemId" .!= ""
      <*> o .:? "Type" .!= ""
      <*> o .:? "StartTicks" .!= 0
      <*> o .:? "EndTicks" .!= 0

-- | The recognised segment types (validated case-insensitively).
segmentTypes :: [Text]
segmentTypes = ["Intro", "Outro", "Preview", "Recap", "Commercial"]

-- | Pure: is @t@ one of 'segmentTypes' (case-insensitive)?
validSegmentType :: Text -> Bool
validSegmentType t = T.toLower t `elem` map T.toLower segmentTypes

-- | Pure: ticks (100 ns) -> seconds.
ticksToSeconds :: Integer -> Double
ticksToSeconds t = fromIntegral t / 1e7

-- | Pure: seconds -> ticks (100 ns), rounded.
secondsToTicks :: Double -> Integer
secondsToTicks s = round (s * 1e7)

-- | Pure validation (milestone 6h): the type must be recognised and the
-- range sane (start >= 0, end > start). 'Left' carries an ASCII message
-- the routes turn into a 400.
validateSegment :: MediaSegment -> Either Text MediaSegment
validateSegment s
  | not (validSegmentType (segType s)) =
      Left ("invalid segment type (expected one of Intro/Outro/Preview/Recap/Commercial): "
            <> segType s)
  | segStartTicks s < 0 = Left "StartTicks must be >= 0"
  | segEndTicks s <= segStartTicks s =
      Left "EndTicks must be greater than StartTicks"
  | otherwise = Right s
