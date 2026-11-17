module FMS.Modules.Metadata.Api.Tmdb
  ( tmdbSearchUrl
  , parseTmdbResults
  , tmdbSearchMovie
  , tmdbSearchTvUrl
  , parseTmdbTvResults
  , tmdbSearchTv
  ) where

import Control.Exception (SomeException, try)
import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON (..), withObject, (.:), (.:?))
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

-- | Build the TMDB @\/search\/movie@ URL for a free-text query. Pure, so it
-- is unit-testable without network access: @tmdbSearchUrl key q@ is
-- @https:\/\/api.themoviedb.org\/3\/search\/movie?api_key=<key>&query=<url-encoded q>@.
tmdbSearchUrl :: Text -> Text -> String
tmdbSearchUrl apiKey query =
  tmdbApiBase ++ "/search/movie?api_key=" ++ T.unpack apiKey
    ++ "&query=" ++ T.unpack (urlEncodeText query)

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
tmdbSearchMovie apiKey query = do
  eres <- tmdbFetch (tmdbSearchUrl apiKey query)
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
tmdbSearchTvUrl apiKey query =
  tmdbApiBase ++ "/search/tv?api_key=" ++ T.unpack apiKey
    ++ "&query=" ++ T.unpack (urlEncodeText query)

-- | One raw hit of @\/search\/tv@.
data TmdbTvItem = TmdbTvItem
  { tvId           :: Int
  , tvName         :: Text
  , tvOverview     :: Maybe Text
  , tvFirstAirDate :: Maybe Text
  , tvVoteAverage  :: Maybe Double
  }

instance FromJSON TmdbTvItem where
  parseJSON = withObject "TmdbTvItem" $ \o ->
    TmdbTvItem
      <$> o .:  "id"
      <*> o .:  "name"
      <*> o .:? "overview"
      <*> o .:? "first_air_date"
      <*> o .:? "vote_average"

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
        , tmdbOverview    = tvOverview i
        , tmdbReleaseYear = tvFirstAirDate i >>= releaseYear
        , tmdbVoteAverage = tvVoteAverage i
        }
    | i <- tmdbTvResults resp
    ]

-- | Search TMDB for TV series matching the query (milestone 4m), same
-- error behaviour as 'tmdbSearchMovie'.
tmdbSearchTv :: Text -> Text -> IO (Either String [TmdbResult])
tmdbSearchTv apiKey query = do
  eres <- tmdbFetch (tmdbSearchTvUrl apiKey query)
  pure (parseTmdbTvResults =<< eres)
