module FMS.Modules.Sessions.Types
  ( PlaySession(..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.!=), (.=))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

-- | One live playback session (docs/todo.org section 6, milestone 6c):
-- a client reporting @POST /Sessions/Playing*@ for an item. Sessions
-- live in memory only (a TVar in the typed registry, key
-- \"sessions.active\"); they are not persisted across restarts.
data PlaySession = PlaySession
  { psId              :: !Text          -- ^ session id (minted server-side)
  , psItemId          :: !Text          -- ^ library item being played
  , psUserId          :: !(Maybe Text)  -- ^ user, when the request carried a valid token
  , psUserName        :: !(Maybe Text)
  , psPositionSeconds :: !Double        -- ^ last reported position
  , psIsPaused        :: !Bool
  , psStartedAt       :: !UTCTime
  , psLastPingAt      :: !UTCTime       -- ^ last Playing/Ping/Progress report
  } deriving (Eq, Show)

-- | API JSON with Jellyfin-style PascalCase keys.
instance ToJSON PlaySession where
  toJSON s = object
    [ "Id"              .= psId s
    , "ItemId"          .= psItemId s
    , "UserId"          .= psUserId s
    , "UserName"        .= psUserName s
    , "PositionSeconds" .= psPositionSeconds s
    , "IsPaused"        .= psIsPaused s
    , "StartedAt"       .= psStartedAt s
    , "LastPingAt"      .= psLastPingAt s
    ]

instance FromJSON PlaySession where
  parseJSON = withObject "PlaySession" $ \o ->
    PlaySession
      <$> o .:  "Id"
      <*> o .:  "ItemId"
      <*> o .:? "UserId"
      <*> o .:? "UserName"
      <*> o .:  "PositionSeconds"
      <*> o .:? "IsPaused" .!= False
      <*> o .:  "StartedAt"
      <*> o .:  "LastPingAt"
