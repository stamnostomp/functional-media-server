module FMS.Modules.Metadata.Api.Tmdb
  ( tmdbSearchUrl
  , parseTmdbResults
  , tmdbSearchMovie
  , tmdbSearchTvUrl
  , parseTmdbTvResults
  , tmdbSearchTv
  , tmdbFetch
  , tmdbSearchUrlAt
  , tmdbSearchMovieAt
  , tmdbSearchTvAt
  , tmdbPopularMoviesUrl
  , tmdbPopularMoviesUrlAt
  , tmdbPopularMovies
  , tmdbPopularMoviesAt
  , tmdbPopularTvUrl
  , tmdbPopularTvUrlAt
  , tmdbPopularTv
  , tmdbPopularTvAt
  , TmdbSeason(..)
  , TmdbEpisode(..)
  , TmdbTvDetails(..)
  , parseTmdbTvDetails
  , parseTmdbSeasonEpisodes
  , tmdbTvDetailsUrlAt
  , tmdbSeasonUrlAt
  , tmdbTvDetailsAt
  , tmdbSeasonAt
  , tmdbMovieDetailsUrlAt
  , tmdbTvDetailsKwUrlAt
  , parseTmdbGenresKeywords
  , tmdbMovieDetailsAt
  , tmdbTvDetailsKwAt
  ) where

import Control.Exception (SomeException, try)
import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON (..), Result (..), Value (..), withObject, (.!=), (.:), (.:?))
import Data.Aeson.Types (parse)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import Network.HTTP.Client
  ( managerResponseTimeout
  , responseBody
  , responseStatus
  , responseTimeoutMicro
  )
import Network.HTTP.Conduit (httpLbs, newManager, parseRequest_)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (statusCode, urlEncode)

import FMS.Modules.Metadata.Api.Types (TmdbResult (..), releaseYear)

-- | Base of the TMDB v3 API.
tmdbApiBase :: String
tmdbApiBase = "https://api.themoviedb.org/3"

-- | Response-header timeout for TMDB calls: 10s in microseconds.
tmdbTimeoutMicros :: Int
tmdbTimeoutMicros = 10000000

-- | Build the TMDB @\/search\/movie@ URL for a free-text query against an
-- explicit API base (pure; a trailing slash on the base is stripped).
-- @tmdbSearchUrlAt base key q@ is @<base>\/search\/movie?api_key=<key>&query=<url-encoded q>@.
tmdbSearchUrlAt :: Text -> Text -> Text -> String
tmdbSearchUrlAt base apiKey query =
  T.unpack (T.dropWhileEnd (== '/') base) ++ "/search/movie?api_key=" ++ T.unpack apiKey
    ++ "&query=" ++ T.unpack (urlEncodeText query)

-- | Build the TMDB @\/search\/movie@ URL against the default API base.
-- Pure, so it is unit-testable without network access: @tmdbSearchUrl key q@ is
-- @https:\/\/api.themoviedb.org\/3\/search\/movie?api_key=<key>&query=<url-encoded q>@.
tmdbSearchUrl :: Text -> Text -> String
tmdbSearchUrl apiKey query = tmdbSearchUrlAt (T.pack tmdbApiBase) apiKey query

-- | Percent-encode a query-string value (http-types 'urlEncode' with
-- 'True', i.e. query-string mode: space becomes @%20@). The 'Text' is UTF-8
-- encoded first; the percent-encoded output is pure ASCII, so decoding it
-- back as latin1 is lossless.
urlEncodeText :: Text -> Text
urlEncodeText = T.pack . BSC.unpack . urlEncode True . TE.encodeUtf8

-- | The @\/search\/movie@ response envelope: just the @results@ array
-- matters to us.
newtype TmdbSearchResponse = TmdbSearchResponse
  { tmdbResults :: [TmdbResult]
  }

instance FromJSON TmdbSearchResponse where
  parseJSON = withObject "TmdbSearchResponse" $ \o ->
    TmdbSearchResponse <$> o .: "results"

-- | Parse the body of a TMDB @\/search\/movie@ response: a JSON object with
-- a @\"results\"@ array of movie objects (see 'TmdbResult'). Pure; invalid
-- JSON, a non-object body, or a missing @results@ key are all 'Left'.
parseTmdbResults :: LBS.ByteString -> Either String [TmdbResult]
parseTmdbResults body = do
  resp <- Aeson.eitherDecode body :: Either String TmdbSearchResponse
  pure (tmdbResults resp)

-- | Search TMDB for movies matching the query (IO client around
-- 'tmdbSearchUrl' + 'parseTmdbResults'). Uses http-conduit with a TLS
-- manager and a 10s response timeout. Never throws: transport errors,
-- timeouts and non-200 statuses all come back as 'Left', a 200 body is
-- decoded by 'parseTmdbResults' (so @{\"results\":[]}@ is @Right []@).
-- (The manager needs no explicit close: http-client >= 0.7 closes it
-- automatically once it is no longer referenced.)
tmdbSearchMovie :: Text -> Text -> IO (Either String [TmdbResult])
tmdbSearchMovie apiKey query = tmdbSearchMovieAt (T.pack tmdbApiBase) apiKey query

-- | Like 'tmdbSearchMovie' but against an explicit API base
-- (FMS_TMDB_BASE_URL), so tests can point it at a mock server.
tmdbSearchMovieAt :: Text -> Text -> Text -> IO (Either String [TmdbResult])
tmdbSearchMovieAt base apiKey query = do
  eres <- tmdbFetch (tmdbSearchUrlAt base apiKey query)
  pure (parseTmdbResults =<< eres)

-- | Shared HTTP GET for the TMDB API: 10s response timeout, 'Left' on
-- transport errors and non-200 statuses (milestone 4m: also used by the
-- TV search).
tmdbFetch :: String -> IO (Either String LBS.ByteString)
tmdbFetch url = do
  manager <- newManager settings
  eres <- try (httpLbs (parseRequest_ url) manager)
  case eres of
    Left (e :: SomeException) ->
      pure (Left ("tmdb request failed (" ++ show e ++ ") for " ++ url))
    Right resp
      | statusCode (responseStatus resp) /= 200 ->
          pure (Left ("tmdb returned HTTP " ++ show (statusCode (responseStatus resp))
                      ++ " for " ++ url))
      | otherwise ->
          pure (Right (responseBody resp))
  where
    settings = tlsManagerSettings
      { managerResponseTimeout = responseTimeoutMicro tmdbTimeoutMicros
      }

-- Milestone 4m: TV search. @/search/tv@ answers @name@/@first_air_date@
-- where the movie endpoint uses @title@/@release_date@; both are mapped
-- onto 'TmdbResult' so callers (e.g. /Items/RemoteSearch?type=tv) share
-- one result shape.

-- | Build the TMDB @\/search\/tv@ URL for a free-text query (pure).
tmdbSearchTvUrl :: Text -> Text -> String
tmdbSearchTvUrl apiKey query = tmdbSearchTvUrlAt (T.pack tmdbApiBase) apiKey query

-- | One raw hit of @\/search\/tv@ (and @\/tv\/popular@, same shape).
data TmdbTvItem = TmdbTvItem
  { tvId           :: Int
  , tvName         :: Text
  , tvOriginalName :: Maybe Text   -- ^ "original_name" (m66)
  , tvOriginalLanguage :: Maybe Text -- ^ "original_language" (m66)
  , tvOverview     :: Maybe Text
  , tvFirstAirDate :: Maybe Text
  , tvVoteAverage  :: Maybe Double
  , tvPoster       :: Maybe Text   -- ^ "poster_path" (m26)
  }

instance FromJSON TmdbTvItem where
  parseJSON = withObject "TmdbTvItem" $ \o ->
    TmdbTvItem
      <$> o .:  "id"
      <*> o .:  "name"
      <*> o .:? "original_name"
      <*> o .:? "original_language"
      <*> o .:? "overview"
      <*> o .:? "first_air_date"
      <*> o .:? "vote_average"
      <*> o .:? "poster_path"

-- | The @\/search\/tv@ response envelope.
newtype TmdbTvSearchResponse = TmdbTvSearchResponse
  { tmdbTvResults :: [TmdbTvItem]
  }

instance FromJSON TmdbTvSearchResponse where
  parseJSON = withObject "TmdbTvSearchResponse" $ \o ->
    TmdbTvSearchResponse <$> o .: "results"

-- | Parse a @\/search\/tv@ body into 'TmdbResult's (name -> title,
-- first_air_date -> release year). Pure; malformed input is 'Left'.
parseTmdbTvResults :: LBS.ByteString -> Either String [TmdbResult]
parseTmdbTvResults body = do
  resp <- Aeson.eitherDecode body :: Either String TmdbTvSearchResponse
  pure
    [ TmdbResult
        { tmdbId          = tvId i
        , tmdbTitle       = tvName i
        , tmdbOriginalTitle    = tvOriginalName i
        , tmdbOriginalLanguage = tvOriginalLanguage i
        , tmdbOverview    = tvOverview i
        , tmdbReleaseYear = tvFirstAirDate i >>= releaseYear
        , tmdbVoteAverage = tvVoteAverage i
        , tmdbPoster      = tvPoster i
        }
    | i <- tmdbTvResults resp
    ]

-- | Search TMDB for TV series matching the query (milestone 4m), same
-- error behaviour as 'tmdbSearchMovie'.
tmdbSearchTv :: Text -> Text -> IO (Either String [TmdbResult])
tmdbSearchTv apiKey query = tmdbSearchTvAt (T.pack tmdbApiBase) apiKey query

-- | Like 'tmdbSearchTv' but against an explicit API base.
tmdbSearchTvAt :: Text -> Text -> Text -> IO (Either String [TmdbResult])
tmdbSearchTvAt base apiKey query = do
  eres <- tmdbFetch (tmdbSearchTvUrlAt base apiKey query)
  pure (parseTmdbTvResults =<< eres)

-- | @\/search\/tv@ URL against an explicit API base (pure).
tmdbSearchTvUrlAt :: Text -> Text -> Text -> String
tmdbSearchTvUrlAt base apiKey query =
  T.unpack (T.dropWhileEnd (== '/') base) ++ "/search/tv?api_key=" ++ T.unpack apiKey
    ++ "&query=" ++ T.unpack (urlEncodeText query)

-- Milestone 26: popular movies/shows for the Discover tab
-- (GET /Discover/Popular). Same envelope as search: @{"results":[...]}@.

-- | Build the TMDB @\/movie\/popular@ URL for a page against an explicit
-- API base (pure): @<base>\/movie\/popular?api_key=<key>&page=<n>@.
tmdbPopularMoviesUrlAt :: Text -> Text -> Int -> String
tmdbPopularMoviesUrlAt base apiKey page =
  T.unpack (T.dropWhileEnd (== '/') base) ++ "/movie/popular?api_key=" ++ T.unpack apiKey
    ++ "&page=" ++ show page

-- | @\/movie\/popular@ URL against the default API base (pure).
tmdbPopularMoviesUrl :: Text -> Int -> String
tmdbPopularMoviesUrl apiKey page = tmdbPopularMoviesUrlAt (T.pack tmdbApiBase) apiKey page

-- | Fetch one page of popular movies from an explicit API base
-- (FMS_TMDB_BASE_URL), parsed with 'parseTmdbResults'. Same never-throws
-- behaviour as 'tmdbSearchMovieAt'.
tmdbPopularMoviesAt :: Text -> Text -> Int -> IO (Either String [TmdbResult])
tmdbPopularMoviesAt base apiKey page = do
  eres <- tmdbFetch (tmdbPopularMoviesUrlAt base apiKey page)
  pure (parseTmdbResults =<< eres)

-- | Popular movies from the default API base.
tmdbPopularMovies :: Text -> Int -> IO (Either String [TmdbResult])
tmdbPopularMovies apiKey page = tmdbPopularMoviesAt (T.pack tmdbApiBase) apiKey page

-- | @\/tv\/popular@ URL against an explicit API base (pure).
tmdbPopularTvUrlAt :: Text -> Text -> Int -> String
tmdbPopularTvUrlAt base apiKey page =
  T.unpack (T.dropWhileEnd (== '/') base) ++ "/tv/popular?api_key=" ++ T.unpack apiKey
    ++ "&page=" ++ show page

-- | @\/tv\/popular@ URL against the default API base (pure).
tmdbPopularTvUrl :: Text -> Int -> String
tmdbPopularTvUrl apiKey page = tmdbPopularTvUrlAt (T.pack tmdbApiBase) apiKey page

-- | Fetch one page of popular TV shows from an explicit API base,
-- parsed with 'parseTmdbTvResults'.
tmdbPopularTvAt :: Text -> Text -> Int -> IO (Either String [TmdbResult])
tmdbPopularTvAt base apiKey page = do
  eres <- tmdbFetch (tmdbPopularTvUrlAt base apiKey page)
  pure (parseTmdbTvResults =<< eres)

-- | Popular TV shows from the default API base.
tmdbPopularTv :: Text -> Int -> IO (Either String [TmdbResult])
tmdbPopularTv apiKey page = tmdbPopularTvAt (T.pack tmdbApiBase) apiKey page

-- Milestone 47: TV series details + season episodes (for the series view).

-- | One entry of the @seasons@ array of @\/tv\/<id>@.
data TmdbSeason = TmdbSeason
  { tsSeasonNumber :: Int
  , tsName         :: Maybe Text
  , tsEpisodeCount :: Maybe Int
  , tsPoster       :: Maybe Text
  } deriving (Show, Eq)

instance FromJSON TmdbSeason where
  parseJSON = withObject "TmdbSeason" $ \o ->
    TmdbSeason
      <$> o .:  "season_number"
      <*> o .:? "name"
      <*> o .:? "episode_count"
      <*> o .:? "poster_path"

-- | One entry of the @episodes@ array of @\/tv\/<id>\/season\/<n>@.
data TmdbEpisode = TmdbEpisode
  { teEpisodeNumber :: Int
  , teName          :: Maybe Text
  , teOverview      :: Maybe Text
  , teAirDate       :: Maybe Text
  , teStill         :: Maybe Text
  , teVote          :: Maybe Double
  } deriving (Show, Eq)

instance FromJSON TmdbEpisode where
  parseJSON = withObject "TmdbEpisode" $ \o ->
    TmdbEpisode
      <$> o .:  "episode_number"
      <*> o .:? "name"
      <*> o .:? "overview"
      <*> o .:? "air_date"
      <*> o .:? "still_path"
      <*> o .:? "vote_average"

-- | The @\/tv\/<id>@ details document (name/overview/first_air_date/poster
-- plus the season list).
data TmdbTvDetails = TmdbTvDetails
  { tdName         :: Maybe Text
  , tdOverview     :: Maybe Text
  , tdFirstAirDate :: Maybe Text
  , tdPoster       :: Maybe Text
  , tdSeasons      :: [TmdbSeason]
  } deriving (Show, Eq)

instance FromJSON TmdbTvDetails where
  parseJSON = withObject "TmdbTvDetails" $ \o ->
    TmdbTvDetails
      <$> o .:? "name"
      <*> o .:? "overview"
      <*> o .:? "first_air_date"
      <*> o .:? "poster_path"
      <*> o .:? "seasons" .!= []

-- | Pure: parse a @\/tv\/<id>@ response body.
parseTmdbTvDetails :: LBS.ByteString -> Either String TmdbTvDetails
parseTmdbTvDetails body = do
  v <- Aeson.eitherDecode body
  case Aeson.fromJSON v of
    Success d -> Right d
    Error e   -> Left e

-- | Pure: parse a @\/tv\/<id>\/season\/<n>@ response body into its episodes.
parseTmdbSeasonEpisodes :: LBS.ByteString -> Either String [TmdbEpisode]
parseTmdbSeasonEpisodes body = do
  v <- Aeson.eitherDecode body
  case parse (withObject "SeasonDoc" (\o -> o .:? "episodes" .!= [])) v of
    Success es -> Right es
    Error e    -> Left e

-- | @\/tv\/<id>?api_key=<key>@ URL against an explicit API base (pure).
tmdbTvDetailsUrlAt :: Text -> Text -> Int -> String
tmdbTvDetailsUrlAt base apiKey tid =
  T.unpack (T.dropWhileEnd (== '/') base) ++ "/tv/" ++ show tid ++ "?api_key=" ++ T.unpack apiKey

-- | @\/tv\/<id>\/season\/<n>?api_key=<key>@ URL (pure).
tmdbSeasonUrlAt :: Text -> Text -> Int -> Int -> String
tmdbSeasonUrlAt base apiKey tid n =
  T.unpack (T.dropWhileEnd (== '/') base) ++ "/tv/" ++ show tid ++ "/season/" ++ show n
    ++ "?api_key=" ++ T.unpack apiKey

-- | Fetch @\/tv\/<id>@ from an explicit base (10s timeout, never throws).
tmdbTvDetailsAt :: Text -> Text -> Int -> IO (Either String TmdbTvDetails)
tmdbTvDetailsAt base apiKey tid = do
  eres <- tmdbFetch (tmdbTvDetailsUrlAt base apiKey tid)
  pure (parseTmdbTvDetails =<< eres)

-- | Fetch @\/tv\/<id>\/season\/<n>@ from an explicit base (never throws).
tmdbSeasonAt :: Text -> Text -> Int -> Int -> IO (Either String [TmdbEpisode])
tmdbSeasonAt base apiKey tid n = do
  eres <- tmdbFetch (tmdbSeasonUrlAt base apiKey tid n)
  pure (parseTmdbSeasonEpisodes =<< eres)

-- Milestone 53a: detail endpoints with genres + keywords
-- (@append_to_response=keywords@).

-- | @\/movie\/<id>?api_key=<key>&append_to_response=keywords@ URL (pure).
tmdbMovieDetailsUrlAt :: Text -> Text -> Int -> String
tmdbMovieDetailsUrlAt base apiKey mid =
  T.unpack (T.dropWhileEnd (== '/') base) ++ "/movie/" ++ show mid
    ++ "?api_key=" ++ T.unpack apiKey ++ "&append_to_response=keywords"

-- | @\/tv\/<id>?api_key=<key>&append_to_response=keywords@ URL (pure).
-- (Unlike 'tmdbTvDetailsUrlAt', which stays plain for the m47 series view.)
tmdbTvDetailsKwUrlAt :: Text -> Text -> Int -> String
tmdbTvDetailsKwUrlAt base apiKey tid =
  T.unpack (T.dropWhileEnd (== '/') base) ++ "/tv/" ++ show tid
    ++ "?api_key=" ++ T.unpack apiKey ++ "&append_to_response=keywords"

-- | Helper: a @{id, name}@ entry of a genres/keywords array; the name only.
newtype TmdbNamed = TmdbNamed (Maybe Text)

instance FromJSON TmdbNamed where
  parseJSON = withObject "TmdbNamed" (\o -> TmdbNamed <$> o .:? "name")

-- | Pure: parse a detail document (@genres: [{id,name}]@ plus the
-- @append_to_response=keywords@ block) into @(genre names, keyword names)@.
-- Movie keywords come as @{"keywords":{"keywords":[...]}}@, TV keywords as
-- @{"keywords":{"results":[...]}}@ — both shapes are accepted. Missing keys
-- yield empty lists; a non-object body is 'Left'.
parseTmdbGenresKeywords :: LBS.ByteString -> Either String ([Text], [Text])
parseTmdbGenresKeywords body = do
  v <- Aeson.eitherDecode body
  case parse parser v of
    Success r -> Right r
    Error e   -> Left e
  where
    parser = withObject "DetailsDoc" $ \o -> do
      gs <- o .:? "genres" .!= ([] :: [Value])
      kwBlock <- o .:? "keywords"
      kws <- case kwBlock of
        Just (Object ko) -> do
          a <- ko .:? "keywords" .!= ([] :: [Value])
          b <- ko .:? "results"  .!= ([] :: [Value])
          pure (a ++ b)
        _ -> pure []
      let names vs = [ n | Success (TmdbNamed (Just n)) <- map Aeson.fromJSON vs ]
      pure (names gs, names kws)

-- | Fetch @\/movie\/<id>?append_to_response=keywords@ (never throws).
tmdbMovieDetailsAt :: Text -> Text -> Int -> IO (Either String ([Text], [Text]))
tmdbMovieDetailsAt base apiKey mid = do
  eres <- tmdbFetch (tmdbMovieDetailsUrlAt base apiKey mid)
  pure (parseTmdbGenresKeywords =<< eres)

-- | Fetch @\/tv\/<id>?append_to_response=keywords@ (never throws).
tmdbTvDetailsKwAt :: Text -> Text -> Int -> IO (Either String ([Text], [Text]))
tmdbTvDetailsKwAt base apiKey tid = do
  eres <- tmdbFetch (tmdbTvDetailsKwUrlAt base apiKey tid)
  pure (parseTmdbGenresKeywords =<< eres)
