module FMS.Modules.Metadata.Api.Types
  ( TmdbResult(..)
  , releaseYear
  ) where

import Data.Aeson (FromJSON (..), withObject, (.:), (.:?))
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

-- | One movie hit from the TMDB @\/search\/movie@ endpoint (milestone 4c,
-- docs/roadmap.org milestone 4). Only the fields the server stores in
-- 'FMS.Modules.Metadata.Types.MediaMetadata' are decoded. 'tmdbReleaseYear'
-- is extracted from the @release_date@ string (@\"YYYY-MM-DD\"@); missing
-- or malformed dates become 'Nothing' (TMDB uses @\"\"@ for unknown).
data TmdbResult = TmdbResult
  { tmdbId          :: Int          -- ^ TMDB's own movie id ("id")
  , tmdbTitle       :: Text         -- ^ "title" (movies) / "name" (tv, mapped)
  , tmdbOverview    :: Maybe Text   -- ^ "overview" (often present, but optional)
  , tmdbReleaseYear :: Maybe Int    -- ^ year of "release_date"/"first_air_date" ("YYYY-MM-DD")
  , tmdbVoteAverage :: Maybe Double -- ^ "vote_average" (community rating 0-10)
  , tmdbPoster      :: Maybe Text   -- ^ "poster_path" (m26; both movie and tv payloads carry it)
  } deriving (Show, Eq)

instance FromJSON TmdbResult where
  parseJSON = withObject "TmdbResult" $ \o -> do
    rid      <- o .:  "id"
    title    <- o .:  "title"
    overview <- o .:? "overview"
    mDate    <- o .:? "release_date"
    vote     <- o .:? "vote_average"
    poster   <- o .:? "poster_path"
    pure TmdbResult
      { tmdbId          = rid
      , tmdbTitle       = title
      , tmdbOverview    = overview
      , tmdbReleaseYear = mDate >>= releaseYear
      , tmdbVoteAverage = vote
      , tmdbPoster      = poster
      }

-- | Extract the year from a TMDB @release_date@ (@\"YYYY-MM-DD\"@): take the
-- part before the first dash and read it as an 'Int'. Empty/malformed
-- strings are 'Nothing'.
releaseYear :: Text -> Maybe Int
releaseYear date =
  case T.splitOn "-" date of
    (y : _) -> readMaybe (T.unpack y)
    []      -> Nothing
