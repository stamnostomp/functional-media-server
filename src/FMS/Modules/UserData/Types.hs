module FMS.Modules.UserData.Types
  ( UserData (..)
  , emptyUserData
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:?), (.!=), (.=))
import Data.Time (UTCTime)

-- | Per-user, per-item data (docs/todo.org section 9, milestone 5c):
-- Jellyfin-style UserData — whether the item is played / favorite, the
-- resume position, how often it was played, and when.
--
-- JSON keys are the Jellyfin-style lowercase names: @played@, @favorite@,
-- @playbackPosition@, @playCount@, @lastPlayedAt@. The 'FromJSON' instance
-- treats every field as optional (defaulting to 'emptyUserData' values), so
-- a partial body like @{\"played\":true}@ decodes fine — the routes merge
-- such partial patches into the stored record.
data UserData = UserData
  { udPlayed           :: !Bool             -- ^ watched to the end
  , udFavorite         :: !Bool             -- ^ liked / favorited
  , udPlaybackPosition :: !(Maybe Double)   -- ^ resume point, seconds
  , udPlayCount        :: !Int              -- ^ times played
  , udLastPlayedAt     :: !(Maybe UTCTime)  -- ^ last play time
  }
  deriving (Show, Eq)

-- | The empty record: not played, not favorite, no position, no plays.
emptyUserData :: UserData
emptyUserData = UserData False False Nothing 0 Nothing

instance ToJSON UserData where
  toJSON u =
    object
      [ "played"           .= udPlayed u
      , "favorite"         .= udFavorite u
      , "playbackPosition" .= udPlaybackPosition u
      , "playCount"        .= udPlayCount u
      , "lastPlayedAt"     .= udLastPlayedAt u
      ]

instance FromJSON UserData where
  parseJSON = withObject "UserData" $ \o ->
    UserData
      <$> o .:? "played"           .!= False
      <*> o .:? "favorite"         .!= False
      <*> o .:? "playbackPosition" .!= Nothing
      <*> o .:? "playCount"        .!= 0
      <*> o .:? "lastPlayedAt"     .!= Nothing
