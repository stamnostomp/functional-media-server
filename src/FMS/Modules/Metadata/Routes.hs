module FMS.Modules.Metadata.Routes
  ( metadataHandler
  , refreshMetadataHandler
  ) where

import Control.Monad.Except (throwError)
import Data.Aeson (object, (.=))
import Data.Text (Text)
import Network.Wai (Request, Response)

import FMS.Core.Module (jsonResponse)
import FMS.Core.Monad (AppError (..), AppM)
import FMS.Core.Types.MediaItem (MediaItem (..))
import FMS.Modules.Library (MonadLibrary (..))
import FMS.Modules.Metadata.Monad (MonadMetadata (..))

-- | @GET \/items\/:id\/metadata@ — the library item merged with its
-- cached metadata:
-- @{\"id\",\"title\",\"path\",\"size\",\"mime\",\"addedAt\",\"metadata\": {...}|null}@.
-- 404 when the item is not in the library; @metadata@ is @null@ until a
-- refresh has found something.
metadataHandler :: [(Text, Text)] -> Request -> AppM Response
metadataHandler captures _req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just i -> do
      mItem <- getItem i
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
refreshMetadataHandler captures _req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just i -> do
      mItem <- getItem i
      case mItem of
        Nothing -> throwError (NotFound ("no library item with id " <> i))
        Just _ -> do
          mMeta <- refreshMetadata i
          case mMeta of
            Nothing -> throwError (NotFound "no metadata found for item")
            Just meta -> pure (jsonResponse meta)
