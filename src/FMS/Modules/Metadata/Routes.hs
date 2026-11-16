module FMS.Modules.Metadata.Routes
  ( metadataHandler
  , refreshMetadataHandler
  , editMetadataHandler
  , genresHandler
  , studiosHandler
  , personsHandler
  , tagsHandler
  , genresFromMetadata
  , studiosFromMetadata
  , personsFromMetadata
  , tagsFromMetadata
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (eitherDecode, object, (.=))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (nub, sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.Wai (Request, Response, strictRequestBody)

import FMS.Core.Module (jsonResponse)
import FMS.Core.Monad (AppError (..), AppM)
import FMS.Core.Types.MediaItem (MediaItem (..))
import FMS.Modules.Auth.Monad (getItemWithAccess)
import FMS.Modules.Metadata.Monad (MonadMetadata (..))
import FMS.Modules.Metadata.Types (MediaMetadata (..), applyManualPatch, emptyMetadata)

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
            [ "id"       .= itemId item
            , "title"    .= itemTitle item
            , "path"     .= itemPath item
            , "size"     .= itemSize item
            , "mime"     .= itemMime item
            , "addedAt"  .= itemAddedAt item
            , "metadata" .= mMeta
            ]))

-- | @POST \/items\/:id\/metadata\/refresh@ — re-read the item's metadata
-- source (currently the NFO sidecar, see "FMS.Modules.Metadata.Nfo") and
-- cache it. 200 with the metadata JSON, 404 when the item is unknown or
-- no metadata could be found ("no metadata found for item").
refreshMetadataHandler :: [(Text, Text)] -> Request -> AppM Response
refreshMetadataHandler captures req =
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
editMetadataHandler captures req =
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
-- (milestone 4h).
tagsHandler :: [(Text, Text)] -> Request -> AppM Response
tagsHandler _captures _req = nameCountsResponse tagsFromMetadata

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
