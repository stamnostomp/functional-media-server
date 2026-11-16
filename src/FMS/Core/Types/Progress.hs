module FMS.Core.Types.Progress
  ( Progress (..)
  , ItemStats (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import Data.Time (UTCTime)

-- | Playback position last reported by a client for one item
-- (docs/modules.org: =Progress= / =ItemStats= are the core stats types).
data Progress = Progress
  { progressCurrentTime :: !Double   -- ^ seconds into the item
  , progressDuration    :: !Double   -- ^ total duration in seconds
  , progressUpdatedAt   :: !UTCTime  -- ^ when the client last reported
  }
  deriving (Show, Eq)

instance ToJSON Progress where
  toJSON p =
    object
      [ "currentTime" .= progressCurrentTime p
      , "duration"    .= progressDuration p
      , "updatedAt"   .= progressUpdatedAt p
      ]

instance FromJSON Progress where
  parseJSON = withObject "Progress" $ \o ->
    Progress
      <$> o .: "currentTime"
      <*> o .: "duration"
      <*> o .: "updatedAt"

-- | Per-item stats accumulated from client activity: how many times the
-- item was played, when it was last played, and (if any) where playback
-- last left off.
data ItemStats = ItemStats
  { statsPlayCount    :: !Int            -- ^ times played from the start
  , statsLastPlayedAt :: !UTCTime        -- ^ last play time
  , statsProgress     :: !(Maybe Progress)  -- ^ last reported position
  }
  deriving (Show, Eq)

instance ToJSON ItemStats where
  toJSON s =
    object
      [ "playCount"    .= statsPlayCount s
      , "lastPlayedAt" .= statsLastPlayedAt s
      , "progress"     .= statsProgress s
      ]

instance FromJSON ItemStats where
  parseJSON = withObject "ItemStats" $ \o ->
    ItemStats
      <$> o .: "playCount"
      <*> o .: "lastPlayedAt"
      <*> o .: "progress"
