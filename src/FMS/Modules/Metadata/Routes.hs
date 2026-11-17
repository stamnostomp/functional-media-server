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
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (FromJSON (..), Value, eitherDecode, object, withObject, (.:?), (.!=), (.=))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (nub, sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.Wai (Request, Response, queryString, strictRequestBody)

import FMS.Core.Module (jsonResponse)
import FMS.Core.Monad (AppError (..), AppM, MonadLog (..), envConfig)
import FMS.Core.Query (queryParams)
import FMS.Core.Types.Config (Config (..))
import FMS.Core.Types.MediaItem (MediaItem (..))
import FMS.Modules.Auth.Monad (currentUserFromReq, getItemWithAccess, requireAdmin, visibleItems)
import FMS.Modules.Library.Monad (MonadLibrary (..))
import FMS.Modules.Metadata.Api.Tmdb (tmdbSearchMovie, tmdbSearchTv)
import FMS.Modules.Metadata.Api.Types (TmdbResult (..))
import FMS.Modules.Metadata.Monad (MonadMetadata (..), refreshAllMetadata)
import FMS.Modules.Metadata.Types (MediaMetadata (..), applyManualPatch, emptyMetadata, mergeTags, removeTag)

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
          mMeta <- refreshMetadata i
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
      cfg <- asks envConfig
      let apiKey = configTmdbApiKey cfg
      if T.null apiKey
        then throwError (BadRequest "MEDIA_TMDB_API_KEY not set")
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
