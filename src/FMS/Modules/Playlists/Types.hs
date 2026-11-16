module FMS.Modules.Playlists.Types
  ( Playlist(..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.!=), (.=))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

-- | One playlist (docs/todo.org section 10, milestone 10a): an ordered
-- list of library item ids with a name. Persisted in
-- @stateDir/playlists.json@.
data Playlist = Playlist
  { plId        :: !Text       -- ^ minted server-side (random hex)
  , plName      :: !Text
  , plItemIds   :: ![Text]     -- ^ library item ids, in playlist order
  , plCreatedAt :: !UTCTime
  } deriving (Eq, Show)

-- | API JSON with Jellyfin-style PascalCase keys.
instance ToJSON Playlist where
  toJSON p = object
    [ "Id"        .= plId p
    , "Name"      .= plName p
    , "ItemIds"   .= plItemIds p
    , "CreatedAt" .= plCreatedAt p
    ]

instance FromJSON Playlist where
  parseJSON = withObject "Playlist" $ \o ->
    Playlist
      <$> o .:  "Id"
      <*> o .:  "Name"
      <*> o .:? "ItemIds" .!= []
      <*> o .:  "CreatedAt"
