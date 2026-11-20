module FMS.Modules.Metadata.Routes
  ( metadataHandler
  , refreshMetadataHandler
  , editMetadataHandler
  , genresHandler
  , studiosHandler
  , personsHandler
  , tagsHandler
  , listItemTagsHandler
  , addItemTagsHandler
  , removeItemTagHandler
  , genresFromMetadata
  , studiosFromMetadata
  , personsFromMetadata
  , tagsFromMetadata
  , refreshAllHandler
  , remoteSearchHandler
  , unmatchedHandler
  , unmatchedItems
  , tmdbResultToSearchHit
  , discoverPopularHandler
  , discoverSearchHandler
  , discoverEntry
  , discoverSection
  , matchDownloaded
  , seriesSearchHandler
  , seriesEpisodesHandler
  , seriesNameOfPayload
  , attachDownloadedLive
  , getMetadataSettingsHandler
  , postMetadataSettingsHandler
  , testMetadataSettingsHandler
  ) where

import Control.Applicative ((<|>))
import Control.Monad (forM, forM_)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), eitherDecode, object, withObject, (.:?), (.!=), (.=))
import Data.HashMap.Strict (HashMap)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.HashMap.Strict as HM
import Data.List (nub, sortOn)
import Data.Maybe (fromMaybe)
import Data.Time.Clock (getCurrentTime)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Network.Wai (Request, Response, queryString, strictRequestBody)
import Text.Read (readMaybe)

import FMS.Core.Module (jsonResponse)
import FMS.Core.Monad (AppError (..), AppM, MonadLog (..), envConfig)
import FMS.Core.Query (queryParams)
import FMS.Core.Types.Config (Config (..))
import FMS.Core.Types.MediaItem (MediaItem (..))
import FMS.Core.Types.User (User (..))
import FMS.Modules.Auth.Monad (currentUserFromReq, getItemWithAccess, requireAdmin, visibleItems)
import FMS.Modules.Library.Monad (MonadLibrary (..))
import FMS.Modules.Metadata.Cache (SeriesCacheEntry (..), cacheFresh, lookupSeriesCache, storeSeriesCache)
import FMS.Modules.Metadata.Api.Tmdb (TmdbEpisode (..), TmdbSeason (..), TmdbTvDetails (..), tmdbPopularMoviesAt, tmdbPopularTvAt, tmdbSearchMovie, tmdbSearchMovieAt, tmdbSearchTv, tmdbSearchTvAt, tmdbSeasonAt, tmdbTvDetailsAt)
import FMS.Modules.Metadata.Api.Types (TmdbResult (..), releaseYear)
import FMS.Modules.Metadata.Monad (MonadMetadata (..), getMetadataSettings, refreshAllMetadata, updateMetadataSettings)
import FMS.Modules.Metadata.Types (MediaMetadata (..), MetadataSettings (..), MetadataSettingsPatch (..), applyManualPatch, emptyMetadata, mergeMetadataSettings, mergeTags, metadataSettingsPublicJson, removeTag)

-- | @GET \/items\/:id\/metadata@ — the library item merged with its
-- cached metadata:
-- @{\"id\",\"title\",\"path\",\"size\",\"mime\",\"addedAt\",\"metadata\": {...}|null}@.
-- 404 when the item is not in the library; @metadata@ is @null@ until a
-- refresh has found something.
metadataHandler :: [(Text, Text)] -> Request -> AppM Response
metadataHandler captures req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just i -> do
      mItem <- getItemWithAccess req i
      case mItem of
        Nothing -> throwError (NotFound ("no library item with id " <> i))
        Just item -> do
          mMeta <- getMetadata i
          pure (jsonResponse (object
            [ "Id"          .= itemId item       -- milestone 17b: PascalCase
            , "Name"        .= itemTitle item    --   item keys, matching
            , "Path"        .= itemPath item     --   MediaItem's ToJSON
            , "Size"        .= itemSize item
            , "MimeType"    .= itemMime item
            , "DateCreated" .= itemAddedAt item
            , "Metadata"    .= mMeta
            ]))

-- | @POST \/items\/:id\/metadata\/refresh@ — re-read the item's metadata
-- source (currently the NFO sidecar, see "FMS.Modules.Metadata.Nfo") and
-- cache it. 200 with the metadata JSON, 404 when the item is unknown or
-- no metadata could be found ("no metadata found for item").
refreshMetadataHandler :: [(Text, Text)] -> Request -> AppM Response
refreshMetadataHandler captures req = do
  requireAdmin req  -- milestone 16: provider refresh is admin-only
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just i -> do
      mItem <- getItemWithAccess req i
      case mItem of
        Nothing -> throwError (NotFound ("no library item with id " <> i))
        Just _ -> do
          -- m57: explicit refresh also re-downloads TMDB artwork (force)
          mMeta <- refreshMetadataForced True i
          case mMeta of
            Nothing -> throwError (NotFound "no metadata found for item")
            Just meta -> pure (jsonResponse meta)

-- | @POST \/items\/:id\/metadata@ — manual metadata edit (milestone 4k,
-- docs/todo.org section 3). The body is a partial metadata object (any
-- subset of the editable keys plus @lockedFields@); 'applyManualPatch'
-- skips fields locked in the stored record and ignores unknown keys.
-- 200 with the updated metadata JSON, 404 unknown item (also enforces
-- folder/policy access via 'getItemWithAccess'), 400 on a non-object body.
editMetadataHandler :: [(Text, Text)] -> Request -> AppM Response
editMetadataHandler captures req = do
  requireAdmin req  -- milestone 16: manual metadata editing is admin-only
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just i -> do
      mItem <- getItemWithAccess req i
      case mItem of
        Nothing -> throwError (NotFound ("no library item with id " <> i))
        Just _ -> do
          body <- liftIO (strictRequestBody req)
          case eitherDecode body of
            Left err -> throwError (BadRequest ("invalid JSON body: " <> T.pack err))
            Right patch -> do
              mOld <- getMetadata i
              let updated = applyManualPatch (fromMaybe emptyMetadata mOld) patch
              setMetadata i updated
              pure (jsonResponse updated)

-- | @GET \/Genres@ — 200 JSON array of @{\"Name\": genre, \"ItemCount\": n}@
-- derived from all stored 'MediaMetadata', grouped by genre and sorted by
-- name (docs/todo.org section 10, milestone 10a). Always 200; with no
-- metadata stored the array is empty.
genresHandler :: [(Text, Text)] -> Request -> AppM Response
genresHandler _captures _req = nameCountsResponse genresFromMetadata

-- | @GET \/Studios@ — same shape as 'genresHandler', over 'metaStudios'
-- (milestone 4h).
studiosHandler :: [(Text, Text)] -> Request -> AppM Response
studiosHandler _captures _req = nameCountsResponse studiosFromMetadata

-- | @GET \/Persons@ — same shape as 'genresHandler', over 'metaPersons'
-- (milestone 4h).
personsHandler :: [(Text, Text)] -> Request -> AppM Response
personsHandler _captures _req = nameCountsResponse personsFromMetadata

-- | @GET \/Tags@ — same shape as 'genresHandler', over 'metaTags'
-- (milestone 4h). Counts are derived live from the stored metadata map
-- ('listAllMetadata'), not cached at scan time, so tag edits are visible
-- immediately.
tagsHandler :: [(Text, Text)] -> Request -> AppM Response
tagsHandler _captures _req = nameCountsResponse tagsFromMetadata

-- Item tags API (custom tags on items; the tags live in the item's
-- metadata record 'metaTags' and therefore survive provider refreshes
-- via 'applyProviderMerge'/'mergeTags'). GET is readable by anyone with
-- access to the item ('getItemWithAccess'); POST/DELETE are admin-only
-- (milestone 16 pattern for metadata edits).

-- | Body of @POST /items/:id/Tags@: @{\"Tags\": [\"a\", \"b\"]@.
newtype TagsRequest = TagsRequest { trTags :: [Text] }

instance FromJSON TagsRequest where
  parseJSON = withObject "TagsRequest" $ \o -> TagsRequest <$> o .:? "Tags" .!= []

-- | @GET \/items\/:id\/Tags@ — 200 @{\"Tags\": [...]@} (empty array when
-- the item has no tags); 404 unknown item.
listItemTagsHandler :: [(Text, Text)] -> Request -> AppM Response
listItemTagsHandler captures req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just i  -> do
      mItem <- getItemWithAccess req i
      case mItem of
        Nothing -> throwError (NotFound ("no library item with id " <> i))
        Just _  -> do
          mMeta <- getMetadata i
          pure (jsonResponse (object ["Tags" .= maybe [] metaTags mMeta]))

-- | @POST \/items\/:id\/Tags@ (admin) — merge the body's tags into the
-- item's stored metadata ('mergeTags': dedupe case-insensitively, keep
-- existing order then append new), persist metadata.json, 200 with the
-- updated full list @{\"Tags\": [...]@}. 404 unknown item, 400 bad JSON.
addItemTagsHandler :: [(Text, Text)] -> Request -> AppM Response
addItemTagsHandler captures req = do
  _ <- requireAdmin req
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just i  -> do
      mItem <- getItemWithAccess req i
      case mItem of
        Nothing -> throwError (NotFound ("no library item with id " <> i))
        Just _  -> do
          body <- liftIO (strictRequestBody req)
          tr <- case eitherDecode body of
            Left err -> throwError (BadRequest ("invalid JSON body: " <> T.pack err))
            Right v  -> pure v
          mOld <- getMetadata i
          let old = fromMaybe emptyMetadata mOld
              merged = old { metaTags = mergeTags (metaTags old) (trTags tr) }
          setMetadata i merged
          pure (jsonResponse (object ["Tags" .= metaTags merged]))

-- | @DELETE \/items\/:id\/Tags\/:tag@ (admin) — remove the tag matching
-- :tag case-insensitively (the capture keeps its original case), persist,
-- 200 with the remaining list. 404 when the item or the tag is unknown.
removeItemTagHandler :: [(Text, Text)] -> Request -> AppM Response
removeItemTagHandler captures req = do
  _ <- requireAdmin req
  case (lookup "id" captures, lookup "tag" captures) of
    (Nothing, _) -> throwError (BadRequest "missing id capture")
    (_, Nothing) -> throwError (BadRequest "missing tag capture")
    (Just i, Just tg) -> do
      mItem <- getItemWithAccess req i
      case mItem of
        Nothing -> throwError (NotFound ("no library item with id " <> i))
        Just _  -> do
          mOld <- getMetadata i
          let old = fromMaybe emptyMetadata mOld
          if not (any (\t -> T.toLower t == T.toLower tg) (metaTags old))
            then throwError (NotFound ("no tag " <> tg <> " on item " <> i))
            else do
              let merged = old { metaTags = removeTag tg (metaTags old) }
              setMetadata i merged
              pure (jsonResponse (object ["Tags" .= metaTags merged]))

-- | Shared shape of the name-count endpoints: 200 array of
-- @{\"Name\": v, \"ItemCount\": n}@ sorted by name.
nameCountsResponse :: ([MediaMetadata] -> [(Text, Int)]) -> AppM Response
nameCountsResponse countFn = do
  metas <- listAllMetadata
  pure (jsonResponse
    [ object ["Name" .= v, "ItemCount" .= n]
    | (v, n) <- countFn metas
    ])

-- | Pure: group a list-valued metadata field into @(value, item count)@
-- pairs, sorted by value. Each record counts once per distinct value
-- (duplicates inside one record are nubbed); empty input -> [].
countMetadataField :: (MediaMetadata -> [Text]) -> [MediaMetadata] -> [(Text, Int)]
countMetadataField get metas =
  sortOn fst (HM.toList counts)
  where
    counts :: HashMap Text Int
    counts = foldl (\m g -> HM.insertWith (+) g 1 m) HM.empty
               (concatMap (nub . get) metas)

-- | Pure: the genres of the given metadata records as @(genre, count)@,
-- sorted by genre name. Used by 'genresHandler' and unit-tested directly.
genresFromMetadata :: [MediaMetadata] -> [(Text, Int)]
genresFromMetadata = countMetadataField metaGenres

-- | Pure (milestone 4h): studios, see 'genresFromMetadata'.
studiosFromMetadata :: [MediaMetadata] -> [(Text, Int)]
studiosFromMetadata = countMetadataField metaStudios

-- | Pure (milestone 4h): persons, see 'genresFromMetadata'.
personsFromMetadata :: [MediaMetadata] -> [(Text, Int)]
personsFromMetadata = countMetadataField metaPersons

-- | Pure (milestone 4h): tags, see 'genresFromMetadata'.
tagsFromMetadata :: [MediaMetadata] -> [(Text, Int)]
tagsFromMetadata = countMetadataField metaTags

-- Milestone 4m (docs/todo.org section 3): library-wide refresh, remote
-- search and the unmatched queue. The routes are registered in
-- "FMS.Modules.Library" (literal segments BEFORE [Lit "items", Capture "id"])
-- because the library module precedes the metadata module in the route
-- table; the handlers live here.

-- | @POST \/Library\/Refresh?replaceAll=true|false@ — refresh the metadata
-- of every library item (admin-only, like the other admin mutations of
-- milestone 16). 200 @{\"refreshed\": n, \"replaceAll\": bool}@.
refreshAllHandler :: [(Text, Text)] -> Request -> AppM Response
refreshAllHandler _captures req = do
  requireAdmin req  -- milestone 16
  let replaceAll = case lookup "replaceall" (queryParams (queryString req)) of
        Just v  -> T.toLower (T.strip v) `elem` ["true", "1", "yes"]
        Nothing -> False
  n <- refreshAllMetadata replaceAll
  pure (jsonResponse (object ["refreshed" .= n, "replaceAll" .= replaceAll]))

-- | @GET \/Items\/RemoteSearch?searchTerm=..&type=movie|tv@ — remote
-- (TMDB) search, answered as a Jellyfin 'RemoteSearchResult' array.
-- Any authenticated user may call it (read-only); 400 on an empty
-- @searchTerm@ or when no TMDB key is configured; a TMDB transport
-- error is logged and answered as an empty array (graceful).
remoteSearchHandler :: [(Text, Text)] -> Request -> AppM Response
remoteSearchHandler _captures req = do
  let qp = queryParams (queryString req)
  case lookup "searchterm" qp of
    Just t | not (T.null (T.strip t)) -> do
      ms <- getMetadataSettings     -- m35: effective (saved) key
      let apiKey = msTmdbApiKey ms
      if T.null apiKey
        then throwError (BadRequest "no TMDB API key set (see /Metadata/Settings)")
        else do
          let isTv  = maybe False (\v -> T.toLower (T.strip v) == "tv") (lookup "type" qp)
              typeName = if isTv then ("Series" :: Text) else "Movie"
              search = if isTv then tmdbSearchTv apiKey t else tmdbSearchMovie apiKey t
          eres <- liftIO search
          case eres of
            Left err -> do
              logErr ("metadata: tmdb search failed: " <> T.pack err)
              pure (jsonResponse ([] :: [Value]))
            Right results ->
              pure (jsonResponse (map (tmdbResultToSearchHit typeName) results))
    _ -> throwError (BadRequest "missing or empty searchTerm")

-- | Pure (milestone 4m): map one TMDB hit to a Jellyfin-shaped
-- RemoteSearchResult object (PascalCase keys).
tmdbResultToSearchHit :: Text -> TmdbResult -> Value
tmdbResultToSearchHit typeName r = object
  [ "Id"             .= T.pack (show (tmdbId r))
  , "Name"           .= tmdbTitle r
  , "Type"           .= typeName
  , "ProductionYear" .= tmdbReleaseYear r
  , "ProviderIds"    .= object ["Tmdb" .= tmdbId r]
  , "Overview"       .= tmdbOverview r
  ]

-- Milestone 26: GET /Discover/Popular — popular movies + shows from TMDB
-- for the UI Discover tab.

-- | @GET \/Discover\/Popular@ — any authenticated user (read-only, like
-- 'remoteSearchHandler'). Fetches page 1 of TMDB /movie/popular and
-- /tv/popular and answers @{\"Movies\":[...],\"Shows\":[...]}@ with at
-- most 20 entries per section. No API key -> logged + 200 with empty
-- arrays; a transport/parse error in one section -> empty array for that
-- section only (graceful, the UI works without TMDB).
discoverPopularHandler :: [(Text, Text)] -> Request -> AppM Response
discoverPopularHandler _captures _req = do
  cfg <- asks envConfig
  ms <- getMetadataSettings       -- m35: effective (saved) key
  let apiKey   = msTmdbApiKey ms
      base     = configTmdbBaseUrl cfg
      imgBase  = configTmdbImageBaseUrl cfg
      imgSize  = configTmdbImageSize cfg
  if T.null apiKey
    then do
      logWarn "metadata: no TMDB API key set (see /Metadata/Settings); /Discover/Popular returns empty arrays"
      pure (jsonResponse (object ["Movies" .= ([] :: [Value]), "Shows" .= ([] :: [Value])]))
    else do
      em <- liftIO (tmdbPopularMoviesAt base apiKey 1)
      es <- liftIO (tmdbPopularTvAt base apiKey 1)
      forM_ (lefts' em) (\e -> logErr ("metadata: tmdb /movie/popular failed: " <> T.pack e))
      forM_ (lefts' es) (\e -> logErr ("metadata: tmdb /tv/popular failed: " <> T.pack e))
      pure (jsonResponse (object
        [ "Movies" .= discoverSection "Movie" imgBase imgSize em
        , "Shows"  .= discoverSection "Series" imgBase imgSize es
        ]))
  where
    lefts' (Left e) = [e]
    lefts' (Right _) = []

-- | @GET \/Discover\/Search?q=...@ — m59: TMDB movie + TV search for the
-- Discover tab (type a query -> TMDB cards; click a card -> the UI searches
-- the indexers for that title). Same response shape as /Discover/Popular:
-- @{\"Movies\":[...],\"Shows\":[...]}@, at most 20 entries per section. No
-- API key or empty query -> 200 with empty arrays; per-section failure ->
-- empty array for that section only.
discoverSearchHandler :: [(Text, Text)] -> Request -> AppM Response
discoverSearchHandler _captures req = do
  cfg <- asks envConfig
  ms <- getMetadataSettings       -- m35: effective (saved) key
  let apiKey  = msTmdbApiKey ms
      base    = configTmdbBaseUrl cfg
      imgBase = configTmdbImageBaseUrl cfg
      imgSize = configTmdbImageSize cfg
      qp      = queryParams (queryString req)
      q       = fromMaybe "" (lookup "q" qp <|> lookup "query" qp)
  if T.null apiKey || T.null q
    then pure (jsonResponse (object ["Movies" .= ([] :: [Value]), "Shows" .= ([] :: [Value])]))
    else do
      em <- liftIO (tmdbSearchMovieAt base apiKey q)
      es <- liftIO (tmdbSearchTvAt base apiKey q)
      forM_ (lefts' em) (\e -> logErr ("metadata: tmdb search/movie failed: " <> T.pack e))
      forM_ (lefts' es) (\e -> logErr ("metadata: tmdb search/tv failed: " <> T.pack e))
      pure (jsonResponse (object
        [ "Movies" .= discoverSection "Movie" imgBase imgSize em
        , "Shows"  .= discoverSection "Series" imgBase imgSize es
        ]))
  where
    lefts' (Left e) = [e]
    lefts' (Right _) = []

-- | Pure (m26): one section of /Discover/Popular — 'Left' (transport or
-- parse error) yields an empty list; 'Right' yields at most 20 entries.
discoverSection :: Text -> Text -> Text -> Either String [TmdbResult] -> [Value]
discoverSection typeName imgBase imgSize eres =
  case eres of
    Left _      -> []
    Right rs    -> map (discoverEntry typeName imgBase imgSize) (take 20 rs)

-- | Pure (m26): map one TMDB hit to the Discover entry JSON
-- (PascalCase): Id/Name/Type/ProductionYear/Overview/VoteAverage/PosterUrl
-- (full image URL when a poster_path is present, else null).
discoverEntry :: Text -> Text -> Text -> TmdbResult -> Value
discoverEntry typeName imgBase imgSize r = object
  [ "Id"             .= T.pack (show (tmdbId r))
  , "Name"           .= tmdbTitle r
  , "Type"           .= typeName
  , "ProductionYear" .= tmdbReleaseYear r
  , "Overview"       .= tmdbOverview r
  , "VoteAverage"    .= tmdbVoteAverage r
  , "PosterUrl"      .= fmap (\p -> imgBase <> "/" <> imgSize <> p) (tmdbPoster r)
  ]

-- | @GET \/Items\/Unmatched@ — the identify/failures queue (milestone 4m):
-- items visible to the requester that have NO metadata record yet.
-- 200 @{\"Items\": [...], \"TotalRecordCount\": n}@.
unmatchedHandler :: [(Text, Text)] -> Request -> AppM Response
unmatchedHandler _captures req = do
  mUser <- currentUserFromReq req
  items <- visibleItems mUser =<< listItems
  metaMap <- listAllMetadataMap
  let unmatched = unmatchedItems (`HM.member` metaMap) items
  pure (jsonResponse (object
    [ "Items" .= unmatched
    , "TotalRecordCount" .= length unmatched
    ]))

-- | Pure (milestone 4m): the items whose id does NOT satisfy the
-- has-metadata predicate (the unmatched queue).
unmatchedItems :: (Text -> Bool) -> [MediaItem] -> [MediaItem]
unmatchedItems hasMeta = filter (not . hasMeta . itemId)

-- m35: TMDB metadata settings (GET/POST /Metadata/Settings, Test) ----------

-- | @GET /Metadata/Settings@ — any authenticated user; the TMDB API key is
-- redacted (null) for non-admins (same pattern as indexer ApiKey).
getMetadataSettingsHandler :: [(Text, Text)] -> Request -> AppM Response
getMetadataSettingsHandler _captures req = do
  mUser <- currentUserFromReq req
  ms <- getMetadataSettings
  pure (jsonResponse (metadataSettingsPublicJson (maybe False isAdmin mUser) ms))

-- | @POST /Metadata/Settings@ — admin only (403 otherwise). Accepts any
-- subset of {TmdbApiKey, FetchImages} (PascalCase or camelCase), merges
-- into the saved settings, persists, returns the full object (caller is
-- admin, so unredacted).
postMetadataSettingsHandler :: [(Text, Text)] -> Request -> AppM Response
postMetadataSettingsHandler _captures req = do
  _ <- requireAdmin req
  body <- liftIO (strictRequestBody req)
  patch <- case eitherDecode body of
    Left err -> throwError (BadRequest ("invalid JSON body: " <> T.pack err))
    Right v  -> pure v
  ms <- updateMetadataSettings (\old -> mergeMetadataSettings old patch)
  pure (jsonResponse (metadataSettingsPublicJson True ms))

-- | @POST /Metadata/Settings/Test@ — admin only. Optionally accepts a
-- settings body to test (not persisted); otherwise tests the effective
-- saved key: a TMDB @/search/movie?query=test@ against
-- 'configTmdbBaseUrl'. Always 200 with @{Ok:true, Results:n}@ or
-- @{Ok:false, Error}@ — never 500.
testMetadataSettingsHandler :: [(Text, Text)] -> Request -> AppM Response
testMetadataSettingsHandler _captures req = do
  _ <- requireAdmin req
  body <- liftIO (strictRequestBody req)
  mPatch <- case eitherDecode body of
    Left _   -> pure Nothing   -- empty/invalid body: test the saved settings
    Right (p :: MetadataSettingsPatch) -> pure (Just p)
  cfg <- asks envConfig
  base <- getMetadataSettings
  let ms = maybe base (\p -> mergeMetadataSettings base p) mPatch
  eres <- liftIO (tmdbSearchMovieAt (configTmdbBaseUrl cfg) (msTmdbApiKey ms) "test")
  pure $ jsonResponse $ case eres of
    Right results -> object [ "Ok" .= True, "Results" .= length results ]
    Left err      -> object [ "Ok" .= False, "Error" .= T.pack err ]

-- | m47: pure matcher -- is season @se@ / episode @ep@ of the series
-- @seriesName@ (case-insensitive) present as a library Episode item?
matchDownloaded :: Text -> Int -> Int -> [MediaItem] -> Bool
matchDownloaded seriesName se ep items =
  any (\i -> T.toLower (itemType i) == "episode"
        && maybe False (\sn -> T.toLower sn == T.toLower seriesName) (itemSeries i)
        && itemSeason i == Just se
        && itemEpisode i == Just ep) items

-- | m47: @GET /Series/Search?name=..@ — TMDB TV search with the effective
-- key; 200 @[{Id,Name,Type,ProductionYear,Overview,VoteAverage,PosterUrl}]@
-- (the 'discoverEntry' shape, Type = "Series"). Empty key or error -> 200 [].
seriesSearchHandler :: [(Text, Text)] -> Request -> AppM Response
seriesSearchHandler _captures req = do
  cfg <- asks envConfig
  ms <- getMetadataSettings
  let qparams = queryParams (queryString req)
      name = fromMaybe "" (lookup "name" qparams <|> lookup "q" qparams)
      apiKey = msTmdbApiKey ms
      imgBase = configTmdbImageBaseUrl cfg
      imgSize = configTmdbImageSize cfg
      refresh = lookup "refresh" qparams == Just "true"   -- m56: force live refetch
      cacheKey = "search:" <> T.toLower (T.strip name)    -- m56
  if T.null apiKey || T.null (T.strip name)
    then pure (jsonResponse ([] :: [Value]))
    else do
      -- m56: serve from the persistent cache when fresh (unless ?refresh=true)
      now <- liftIO getCurrentTime
      mCached <- lookupSeriesCache cacheKey
      case mCached of
        Just e | not refresh && cacheFresh now (configSeriesCacheTtlSec cfg) e ->
          pure (jsonResponse (scePayload e))
        _ -> do
          res <- liftIO (tmdbSearchTvAt (configTmdbBaseUrl cfg) apiKey (T.strip name))
          case res of
            Left err -> do
              logWarn ("series search for \"" <> name <> "\" failed: " <> T.pack err)
              pure (jsonResponse ([] :: [Value]))
            Right rs -> do
              let payload = toJSON (map (discoverEntry "Series" imgBase imgSize) (take 20 rs))
              storeSeriesCache cacheKey payload   -- cache successful payloads only
              pure (jsonResponse payload)

-- | m47: @GET /Series/:id/Episodes@ — TMDB @\/tv\/<id>@ plus every season's
-- episodes, each flagged @Downloaded@ when a matching Episode item exists in
-- the library. Graceful: no key / any TMDB error -> 200 with empty Seasons.
seriesEpisodesHandler :: [(Text, Text)] -> Request -> AppM Response
seriesEpisodesHandler captures req = do
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just tidTxt -> case readMaybe (T.unpack tidTxt) of
      Nothing -> throwError (BadRequest "invalid series id")
      Just tid -> do
        cfg <- asks envConfig
        ms <- getMetadataSettings
        let apiKey = msTmdbApiKey ms
            base = configTmdbBaseUrl cfg
            imgBase = configTmdbImageBaseUrl cfg
            imgSize = configTmdbImageSize cfg
            qparams = queryParams (queryString req)
            refresh = lookup "refresh" qparams == Just "true"   -- m56
        if T.null apiKey
          then pure (jsonResponse (emptySeriesDetail tidTxt))
          else do
            items <- listItems
            -- m56: serve the cached raw payload (Downloaded attached live below)
            -- when fresh, unless ?refresh=true
            now <- liftIO getCurrentTime
            mCached <- lookupSeriesCache tidTxt
            case mCached of
              Just e | not refresh && cacheFresh now (configSeriesCacheTtlSec cfg) e ->
                pure (jsonResponse (attachDownloadedLive (seriesNameOfPayload (scePayload e)) (scePayload e) items))
              _ -> fetchAndCacheEpisodes apiKey base imgBase imgSize tid tidTxt items

-- | m56: fetch the TMDB tv details + every season, build the RAW payload
-- (no Downloaded flags -- those are attached live per request), cache it
-- under the tmdb id, and answer with Downloaded attached.
fetchAndCacheEpisodes
  :: Text -> Text -> Text -> Text -> Int -> Text -> [MediaItem] -> AppM Response
fetchAndCacheEpisodes apiKey base imgBase imgSize tid tidTxt items = do
  ed <- liftIO (tmdbTvDetailsAt base apiKey tid)
  case ed of
    Left err -> do
      logWarn ("series details for " <> tidTxt <> " failed: " <> T.pack err)
      pure (jsonResponse (emptySeriesDetail tidTxt))
    Right d -> do
      let seriesName = fromMaybe "" (tdName d)
          posterUrl = fmap (\p -> imgBase <> "/" <> imgSize <> p) (tdPoster d)
          year = tdFirstAirDate d >>= releaseYear
          seasonsSorted = sortOn (\sn -> (tsSeasonNumber sn == 0, tsSeasonNumber sn)) (tdSeasons d)
      seasonVals <- forM seasonsSorted $ \sn -> do
        let n = tsSeasonNumber sn
        eres <- liftIO (tmdbSeasonAt base apiKey tid n)
        eps <- case eres of
          Left err -> do
            logWarn ("series season " <> T.pack (show n) <> " failed: " <> T.pack err)
            pure []
          Right es -> pure
            [ object
                [ "EpisodeNumber" .= teEpisodeNumber e
                , "Name"          .= teName e
                , "Overview"      .= teOverview e
                , "AirDate"       .= teAirDate e
                , "StillUrl"      .= fmap (\p -> imgBase <> "/" <> imgSize <> p) (teStill e)
                ]
            | e <- es ]
        pure (object
          [ "SeasonNumber" .= n
          , "Name"         .= tsName sn
          , "Episodes"     .= eps ])
      let rawPayload = object
            [ "Id" .= tidTxt
            , "Name" .= tdName d
            , "Year" .= year
            , "Overview" .= tdOverview d
            , "PosterUrl" .= posterUrl
            , "Seasons" .= seasonVals ]
      storeSeriesCache tidTxt rawPayload   -- m56: cache the raw payload
      pure (jsonResponse (attachDownloadedLive seriesName rawPayload items))

-- | m56: empty Episodes payload for handlers when the TMDB key is missing
-- or the fetch failed (never a 500).
emptySeriesDetail :: Text -> Value
emptySeriesDetail tidTxt = object
  [ "Id" .= tidTxt, "Name" .= (Nothing :: Maybe Text), "Year" .= (Nothing :: Maybe Int)
  , "Overview" .= (Nothing :: Maybe Text), "PosterUrl" .= (Nothing :: Maybe Text)
  , "Seasons" .= ([] :: [Value]) ]

-- | m56 pure: the series name stored in a cached Episodes payload.
seriesNameOfPayload :: Value -> Text
seriesNameOfPayload v = case v of
  Object o -> case KeyMap.lookup (Key.fromText "Name") o of
    Just (String t) -> t
    _               -> ""
  _ -> ""

-- | m56 pure: attach live @Downloaded@ flags to every episode of a (cached
-- or fresh) Episodes payload, matching the library per request.
attachDownloadedLive :: Text -> Value -> [MediaItem] -> Value
attachDownloadedLive seriesName payload items = case payload of
  Object o -> case KeyMap.lookup (Key.fromText "Seasons") o of
    Just (Array seasons) ->
      Object (KeyMap.insert (Key.fromText "Seasons") (Array (V.fromList (map seasonV (V.toList seasons)))) o)
    _ -> payload
  _ -> payload
  where
    seasonV sv = case sv of
      Object so -> case KeyMap.lookup (Key.fromText "SeasonNumber") so of
        Just (Number snNum) -> case KeyMap.lookup (Key.fromText "Episodes") so of
          Just (Array eps) ->
            Object (KeyMap.insert (Key.fromText "Episodes")
                     (Array (V.fromList (map (epV (round snNum)) (V.toList eps)))) so)
          _ -> sv
        _ -> sv
      _ -> sv
    epV sn ev = case ev of
      Object eo -> case KeyMap.lookup (Key.fromText "EpisodeNumber") eo of
        Just (Number en) ->
          Object (KeyMap.insert (Key.fromText "Downloaded") (toJSON (matchDownloaded seriesName sn (round en) items)) eo)
        _ -> ev
      _ -> ev
