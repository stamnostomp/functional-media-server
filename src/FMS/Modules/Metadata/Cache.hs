module FMS.Modules.Metadata.Cache
  ( SeriesCache
  , SeriesCacheEntry (..)
  , seriesCacheKey
  , emptySeriesCache
  , cacheFresh
  , cacheGet
  , cachePut
  , loadSeriesCache
  , saveSeriesCache
  , loadSeriesCacheIntoState
  , lookupSeriesCache
  , storeSeriesCache
  , seriesCachePath
  ) where

import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, (.:), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import FMS.Core.Monad (AppM, MonadLog (..), envConfig, getState, putState)
import FMS.Core.Types.Config (Config (..))

-- | m56: persistent TMDB series-metadata cache at
-- @<stateDir>\/series-cache.json@ so @\/Series\/Search@ and
-- @\/Series\/:id\/Episodes@ do not hit TMDB on every call.
-- Keys: @\"<tmdbId>\"@ for the Episodes payload, @\"search:<query>\"@ for
-- search arrays. Only successful payloads are cached; 'Downloaded' flags
-- are NOT stored (computed live per request from the library).

data SeriesCacheEntry = SeriesCacheEntry
  { sceFetchedAt :: !UTCTime  -- ^ when the payload was fetched (iso8601 in JSON)
  , scePayload   :: !Value    -- ^ the raw response payload
  } deriving (Show, Eq)

instance ToJSON SeriesCacheEntry where
  toJSON e = object
    [ "fetchedAt" .= sceFetchedAt e
    , "payload"   .= scePayload e
    ]

instance FromJSON SeriesCacheEntry where
  parseJSON = withObject "SeriesCacheEntry" $ \o ->
    SeriesCacheEntry <$> o .: "fetchedAt" <*> o .: "payload"

type SeriesCache = HashMap Text SeriesCacheEntry

seriesCacheKey :: Text
seriesCacheKey = "metadata.seriesCache"

emptySeriesCache :: SeriesCache
emptySeriesCache = HM.empty

seriesCachePath :: FilePath -> FilePath
seriesCachePath stateDir = stateDir </> "series-cache.json"

-- | Pure (m56): is the entry fresh at @now@ with TTL @ttlSec@?
-- ttl <= 0 means "always refetch" (never fresh).
cacheFresh :: UTCTime -> Int -> SeriesCacheEntry -> Bool
cacheFresh _ ttlSec _ | ttlSec <= 0 = False
cacheFresh now ttlSec e = diffUTCTime now (sceFetchedAt e) < fromIntegral ttlSec

-- | Pure: look up a key in the cache map.
cacheGet :: Text -> SeriesCache -> Maybe SeriesCacheEntry
cacheGet = HM.lookup

-- | Pure: insert/overwrite a key (with a fresh timestamp supplied by the caller).
cachePut :: Text -> UTCTime -> Value -> SeriesCache -> SeriesCache
cachePut k now payload = HM.insert k (SeriesCacheEntry now payload)

-- | Load the persisted cache; a missing/malformed file yields an empty map
-- (never crashes).
loadSeriesCache :: FilePath -> IO SeriesCache
loadSeriesCache path = do
  exists <- doesFileExist path
  if not exists
    then pure emptySeriesCache
    else do
      result <- try (LBS.readFile path) :: IO (Either SomeException LBS.ByteString)
      pure $ case result of
        Left _      -> emptySeriesCache
        Right bytes -> fromMaybe emptySeriesCache (Aeson.decode bytes)

-- | Persist the whole cache (strict write, like saveMetadata).
saveSeriesCache :: FilePath -> SeriesCache -> IO ()
saveSeriesCache path = LBS.writeFile path . Aeson.encode

-- | Boot: load the cache file into a TVar in the typed state registry.
loadSeriesCacheIntoState :: AppM ()
loadSeriesCacheIntoState = do
  cfg <- asks envConfig
  cache <- liftIO (loadSeriesCache (seriesCachePath (configStateDir cfg)))
  putState seriesCacheKey cache

lookupSeriesCache :: Text -> AppM (Maybe SeriesCacheEntry)
lookupSeriesCache k = do
  mTv <- getState seriesCacheKey
  case mTv of
    Nothing -> pure Nothing
    Just cache -> pure (cacheGet k cache)

-- | Store a freshly fetched payload: update the TVar and persist to disk
-- (best-effort: a save failure is logged, never thrown).
storeSeriesCache :: Text -> Value -> AppM ()
storeSeriesCache k payload = do
  cfg <- asks envConfig
  now <- liftIO getCurrentTime
  mTv <- getState seriesCacheKey
  case mTv of
    Nothing -> pure ()
    Just cache -> do
      let cache' = cachePut k now payload cache
      putState seriesCacheKey cache'
      r <- liftIO (try (saveSeriesCache (seriesCachePath (configStateDir cfg)) cache'))
      case r of
        Left e  -> logWarn ("series cache: cannot persist: " <> T.pack (show (e :: SomeException)))
        Right _ -> pure ()
