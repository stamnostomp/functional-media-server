module FMS.Modules.Collections.Routes
  ( collectionsRoutes
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), eitherDecode, encode, object, withObject, (.:), (.:?), (.!=), (.=))
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types (status201)
import Network.Wai (Request, Response, responseLBS, strictRequestBody)

import FMS.Core.Module (Route, Segment (..), delete, get, jsonResponse, post)
import FMS.Core.Monad (AppError (..), AppM)
import FMS.Core.Types.MediaItem (MediaItem (..))
import FMS.Modules.Auth.Monad (currentUserFromReq, visibleItems)
import FMS.Modules.Collections.Monad (MonadCollections (..))
import FMS.Modules.Collections.Types (Collection (..))
import FMS.Modules.Library (MonadLibrary (..))

-- | The collections (box sets) API (milestone 7), Jellyfin-shaped and
-- mirroring the playlists module: list/create/get/delete collections plus
-- per-collection item add (batch and single)/remove/list. All responses
-- are JSON; bad bodies are 400, unknown collections or items are 404.
collectionsRoutes :: [Route]
collectionsRoutes =
  [ get    [Lit "Collections"] listCollectionsHandler
  , post   [Lit "Collections"] createCollectionHandler
  , get    [Lit "Collections", Capture "id"] getCollectionHandler
  , delete [Lit "Collections", Capture "id"] deleteCollectionHandler
  , get    [Lit "Collections", Capture "id", Lit "Items"] collectionItemsHandler
  , post   [Lit "Collections", Capture "id", Lit "Items"] addItemsHandler
  , post   [Lit "Collections", Capture "id", Lit "Items", Capture "itemId"] addItemHandler
  , delete [Lit "Collections", Capture "id", Lit "Items", Capture "itemId"] removeItemHandler
  ]

-- | Body of @POST /Collections@:
-- @{\"Name\": \"...\", \"ItemIds\"?: [\"1\", ...]}@.
data CreateCollectionRequest = CreateCollectionRequest
  { ccrName    :: Text
  , ccrItemIds :: [Text]
  }

instance FromJSON CreateCollectionRequest where
  parseJSON = withObject "CreateCollectionRequest" $ \o ->
    CreateCollectionRequest
      <$> o .:  "Name"
      <*> o .:? "ItemIds" .!= []

-- | Body of @POST /Collections/:id/Items@: @{\"Ids\": [\"1\", ...]}@.
data AddItemsRequest = AddItemsRequest
  { airIds :: [Text] }

instance FromJSON AddItemsRequest where
  parseJSON = withObject "AddItemsRequest" $ \o ->
    AddItemsRequest <$> o .:? "Ids" .!= []

-- Handlers ------------------------------------------------------------------

-- | @GET /Collections@ — 200 JSON array of all collections (oldest first).
listCollectionsHandler :: [(Text, Text)] -> Request -> AppM Response
listCollectionsHandler _captures _req =
  jsonResponse <$> listCollections

-- | @POST /Collections@ — create a collection. 400 on a malformed body or
-- an empty name; unknown item ids are dropped by 'createCollection'.
-- Otherwise 201 with the new collection's JSON.
createCollectionHandler :: [(Text, Text)] -> Request -> AppM Response
createCollectionHandler _captures req = do
  body <- liftIO (strictRequestBody req)
  case eitherDecode body of
    Left err -> throwError (BadRequest ("invalid JSON body: " <> T.pack err))
    Right ccr
      | T.null (T.strip (ccrName ccr)) ->
          throwError (BadRequest "collection name must not be empty")
      | otherwise -> do
          c <- createCollection (ccrName ccr) (ccrItemIds ccr)
          pure (responseLBS status201
                  [("Content-Type", "application/json")] (encode c))

-- | @GET /Collections/:id@ — 200 the collection JSON, 404 when unknown.
getCollectionHandler :: [(Text, Text)] -> Request -> AppM Response
getCollectionHandler captures _req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just cid -> do
      mC <- getCollection cid
      case mC of
        Nothing -> throwError (NotFound ("no collection with id " <> cid))
        Just c  -> pure (jsonResponse c)

-- | @DELETE /Collections/:id@ — 200 @{\"status\":\"ok\"@}, 404 when unknown.
deleteCollectionHandler :: [(Text, Text)] -> Request -> AppM Response
deleteCollectionHandler captures _req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just cid -> do
      ok <- deleteCollection cid
      if ok
        then pure (jsonResponse (object ["status" .= ("ok" :: Text)]))
        else throwError (NotFound ("no collection with id " <> cid))

-- | @GET /Collections/:id/Items@ — 200 the collection's library items as
-- a JSON array, in collection order. Ids whose item no longer exists are
-- skipped, and (milestone 5d) only items visible to the requester are
-- returned. 404 when the collection is unknown.
collectionItemsHandler :: [(Text, Text)] -> Request -> AppM Response
collectionItemsHandler captures req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just cid -> do
      mC <- getCollection cid
      case mC of
        Nothing -> throwError (NotFound ("no collection with id " <> cid))
        Just c  -> do
          mUser <- currentUserFromReq req
          visible <- listItems >>= visibleItems mUser
          let visById = HM.fromList [(itemId i, i) | i <- visible]
          pure (jsonResponse [ i | iid <- colItemIds c, Just i <- [HM.lookup iid visById] ])

-- | @POST /Collections/:id/Items@ with body @{\"Ids\": [...]@} — add the
-- given items to the collection (deduped, order kept). Unknown item ids
-- are dropped silently. 400 on a malformed body, 404 when the collection
-- is unknown; otherwise 200 with the updated collection JSON.
addItemsHandler :: [(Text, Text)] -> Request -> AppM Response
addItemsHandler captures req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just cid -> do
      body <- liftIO (strictRequestBody req)
      case eitherDecode body of
        Left err -> throwError (BadRequest ("invalid JSON body: " <> T.pack err))
        Right air -> do
          mC <- getCollection cid
          case mC of
            Nothing -> throwError (NotFound ("no collection with id " <> cid))
            Just _ -> do
              allItems <- listItems
              let known = HM.fromList [(itemId i, ()) | i <- allItems]
                  valid = filter (`HM.member` known) (airIds air)
              _ <- addItemsToCollection cid valid
              mUpdated <- getCollection cid
              case mUpdated of
                Just c  -> pure (jsonResponse c)
                Nothing -> throwError (InternalError "collection vanished after update")

-- | @POST /Collections/:id/Items/:itemId@ — add one item to the
-- collection (deduped). 404 when the collection or the item is unknown;
-- otherwise 200 with the updated collection JSON.
addItemHandler :: [(Text, Text)] -> Request -> AppM Response
addItemHandler captures _req =
  case (lookup "id" captures, lookup "itemId" captures) of
    (Nothing, _) -> throwError (BadRequest "missing id capture")
    (_, Nothing) -> throwError (BadRequest "missing itemId capture")
    (Just cid, Just iid) -> do
      mC <- getCollection cid
      case mC of
        Nothing -> throwError (NotFound ("no collection with id " <> cid))
        Just _ -> do
          mItem <- getItem iid
          case mItem of
            Nothing -> throwError (NotFound ("no library item with id " <> iid))
            Just _ -> do
              _ <- addItemsToCollection cid [iid]
              mUpdated <- getCollection cid
              case mUpdated of
                Just c  -> pure (jsonResponse c)
                Nothing -> throwError (InternalError "collection vanished after update")

-- | @DELETE /Collections/:id/Items/:itemId@ — remove the item from the
-- collection. 404 when the collection or the item is unknown; otherwise
-- 200 with the updated collection JSON.
removeItemHandler :: [(Text, Text)] -> Request -> AppM Response
removeItemHandler captures _req =
  case (lookup "id" captures, lookup "itemId" captures) of
    (Nothing, _) -> throwError (BadRequest "missing id capture")
    (_, Nothing) -> throwError (BadRequest "missing itemId capture")
    (Just cid, Just iid) -> do
      mC <- getCollection cid
      case mC of
        Nothing -> throwError (NotFound ("no collection with id " <> cid))
        Just _ -> do
          mItem <- getItem iid
          case mItem of
            Nothing -> throwError (NotFound ("no library item with id " <> iid))
            Just _ -> do
              _ <- removeItemFromCollection cid iid
              mUpdated <- getCollection cid
              case mUpdated of
                Just c  -> pure (jsonResponse c)
                Nothing -> throwError (InternalError "collection vanished after update")
