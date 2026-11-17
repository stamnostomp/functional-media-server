module FMS.Core.Types.Progress
  ( Progress (..)
  , ItemStats (..)
  , progressPercent
  , simpleProgressEntry
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, (.:), (.:?), (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Time (UTCTime)

import FMS.Core.Types.MediaItem (MediaItem (..), simpleItemJson)

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

-- | Pure: watch percentage 0..100 from a reported position —
-- @floor (currentTime / duration * 100)@ clamped to [0, 100]; 0 when the
-- duration is unknown (<= 0). Used by the legacy /continuewatching
-- endpoint for the stamno-media UI's @progress@ int field.
progressPercent :: Double -> Double -> Int
progressPercent cur dur
  | dur <= 0  = 0
  | otherwise = max 0 (min 100 (floor (cur / dur * 100) :: Int))

-- | The legacy simple continue-watching entry for the stamno-media UI:
-- 'simpleItemJson' plus a lowercase @progress@ percent int.
simpleProgressEntry :: MediaItem -> Progress -> Value
simpleProgressEntry item p = case simpleItemJson item of
  Object o -> Object (KeyMap.insert (Key.fromText "progress")
                     (toJSON (progressPercent (progressCurrentTime p) (progressDuration p))) o)
  v        -> v
