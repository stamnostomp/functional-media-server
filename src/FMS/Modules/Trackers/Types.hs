module FMS.Modules.Trackers.Types
  ( Tracker (..)
  , trackerPublicJson
  , Release (..)
  , sortReleasesBySeeders
  , dedupeReleases
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, (.:), (.:?), (.!=), (.=))
import qualified Data.Aeson as Aeson
import Data.List (nubBy, sortBy)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import qualified Data.Text as T

-- | One Jackett-style indexer (m20b). The API key is SECRET: it is only
-- ever sent to the tracker itself and is redacted from non-admin API
-- responses ('trackerPublicJson').
data Tracker = Tracker
  { trackerId      :: !Text
  , trackerName    :: !Text
  , trackerUrl     :: !Text   -- ^ base URL, e.g. http://127.0.0.1:9117
  , trackerApiKey  :: !Text
  , trackerEnabled :: !Bool
  } deriving (Eq, Show)

instance ToJSON Tracker where
  toJSON t = object
    [ "Id"      .= trackerId t
    , "Name"    .= trackerName t
    , "Url"     .= trackerUrl t
    , "ApiKey"  .= trackerApiKey t
    , "Enabled" .= trackerEnabled t
    ]

instance FromJSON Tracker where
  parseJSON = withObject "Tracker" $ \o ->
    Tracker
      <$> o .:  "Id"
      <*> o .:  "Name"
      <*> o .:  "Url"
      <*> o .:? "ApiKey" .!= ""
      <*> o .:? "Enabled" .!= True

-- | API JSON with the ApiKey redacted (null) for non-admins; admins get
-- the full record. SECURITY: never expose the key to non-admins.
trackerPublicJson :: Bool -> Tracker -> Value
trackerPublicJson isAdmin t
  | isAdmin   = toJSON t
  | otherwise = object
      [ "Id"      .= trackerId t
      , "Name"    .= trackerName t
      , "Url"     .= trackerUrl t
      , "ApiKey"  .= Aeson.Null
      , "Enabled" .= trackerEnabled t
      ]

-- | One Torznab search result (parsed from Jackett XML, m20b).
data Release = Release
  { relTitle       :: !Text
  , relSizeBytes   :: !(Maybe Integer)
  , relSeeders     :: !(Maybe Int)
  , relDownloadUrl :: !(Maybe Text)  -- ^ .torrent enclosure / "download" attr
  , relMagnet      :: !(Maybe Text)  -- ^ "magneturl" attr
  } deriving (Eq, Show)

instance ToJSON Release where
  toJSON r = object
    [ "Title"       .= relTitle r
    , "SizeBytes"   .= relSizeBytes r
    , "Seeders"     .= relSeeders r
    , "DownloadUrl" .= relDownloadUrl r
    , "Magnet"      .= relMagnet r
    ]

-- | Default search-result order (m25): seeders descending, missing
-- seeders last; ties broken by size descending. Generic in the payload so
-- it can sort @[(Indexer, Release)]@ pairs too.
sortReleasesBySeeders :: [(a, Release)] -> [(a, Release)]
sortReleasesBySeeders =
  sortBy (comparing (\( _, r) -> (Down (relSeeders r), Down (relSizeBytes r))))

-- | m66: dedupe releases by LOWERCASED title, keeping the FIRST
-- occurrence. Used when one /Indexers/Search runs several queries (the
-- English title AND the original-language title, see the Discover flow
-- in stamno-media): the same release can match more than one query and
-- must appear only once. Call AFTER 'sortReleasesBySeeders' so the
-- best-seeded copy of a duplicated release is the one kept.
dedupeReleases :: [(a, Release)] -> [(a, Release)]
dedupeReleases = nubBy (\(_, r1) (_, r2) -> T.toLower (relTitle r1) == T.toLower (relTitle r2))
