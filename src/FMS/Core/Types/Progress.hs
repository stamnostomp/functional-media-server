module FMS.Core.Types.Progress
  ( Progress (..)
  , ItemStats (..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.=))
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
      [ "PositionSeconds" .= progressCurrentTime p   -- milestone 17c: PascalCase
      , "DurationSeconds" .= progressDuration p
      , "UpdatedAt"       .= progressUpdatedAt p
      ]

instance FromJSON Progress where
  -- milestone 17c: accept the new PascalCase keys AND the old lowercase
  -- ones, so pre-17c stats.json files still load
  parseJSON = withObject "Progress" $ \o -> do
    -- (.:? succeeds with Nothing on an absent key, so fall through manually)
    let get2 new old = do
          mv <- o .:? new
          case mv of
            Just v  -> pure v
            Nothing -> o .:? old
    ct <- get2 "PositionSeconds" "currentTime"
    du <- get2 "DurationSeconds" "duration"
    ua <- get2 "UpdatedAt" "updatedAt"
    case (ct, du, ua) of
      (Just a, Just b, Just c) -> pure (Progress a b c)
      _ -> fail "Progress needs PositionSeconds/DurationSeconds/UpdatedAt"

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
