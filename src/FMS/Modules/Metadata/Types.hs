module FMS.Modules.Metadata.Types
  ( MediaMetadata(..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.!=), (.:?), (.=))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)

-- | Metadata for one library item (docs/roadmap.org milestone 4,
-- docs/todo.org section 3). All fields are optional because a source (NFO,
-- later TMDB/TvMaze) may only know some of them. 'metaProviderIds' maps a
-- provider name ("tmdb", "imdb", ...) to the id the item has there;
-- 'metaSource' records where the metadata came from ("nfo", later "tmdb").
data MediaMetadata = MediaMetadata
  { metaTitle          :: Maybe Text          -- ^ <title>
  , metaOverview       :: Maybe Text          -- ^ <overview> / plot
  , metaGenres         :: [Text]              -- ^ all <genre> elements
  , metaStudios        :: [Text]              -- ^ all <studio> elements (milestone 4h)
  , metaPersons        :: [Text]              -- ^ actor/director/person names (milestone 4h)
  , metaTags           :: [Text]              -- ^ all <tag> elements (milestone 4h)
  , metaYear           :: Maybe Int           -- ^ <year>
  , metaRating         :: Maybe Double        -- ^ <rating>
  , metaRuntimeMinutes :: Maybe Int           -- ^ <runtime> in minutes
  , metaProviderIds    :: HashMap Text Text   -- ^ <tmdbid> -> "tmdb", <imdbid> -> "imdb"
  , metaSource         :: Text                -- ^ provenance, e.g. "nfo"
  } deriving (Show, Eq)

instance ToJSON MediaMetadata where
  toJSON m = object
    [ "title"          .= metaTitle m
    , "overview"       .= metaOverview m
    , "genres"         .= metaGenres m
    , "studios"        .= metaStudios m
    , "persons"        .= metaPersons m
    , "tags"           .= metaTags m
    , "year"           .= metaYear m
    , "rating"         .= metaRating m
    , "runtimeMinutes" .= metaRuntimeMinutes m
    , "providerIds"    .= metaProviderIds m
    , "source"         .= metaSource m
    ]

instance FromJSON MediaMetadata where
  parseJSON = withObject "MediaMetadata" $ \o ->
    MediaMetadata
      <$> o .:? "title"
      <*> o .:? "overview"
      <*> o .:? "genres" .!= []
      <*> o .:? "studios" .!= []      -- milestone 4h; absent in pre-4h metadata.json
      <*> o .:? "persons" .!= []
      <*> o .:? "tags" .!= []
      <*> o .:? "year"
      <*> o .:? "rating"
      <*> o .:? "runtimeMinutes"
      <*> o .:? "providerIds" .!= HM.empty
      <*> o .:? "source" .!= "unknown"
