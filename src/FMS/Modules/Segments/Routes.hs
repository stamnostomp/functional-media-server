module FMS.Modules.Segments.Routes
  ( segmentsRoutes
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), eitherDecode, object, withObject, (.:), (.:?), (.!=), (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Network.Wai (Request, Response, strictRequestBody)

import FMS.Core.Module (Route, Segment (..), delete, get, jsonResponse, post)
import FMS.Core.Monad (AppError (..), AppM)
import FMS.Modules.Auth.Monad (getItemWithAccess)
import FMS.Modules.Segments.Monad (MonadSegments (..), newSegmentId)
import FMS.Modules.Segments.Types (MediaSegment (..), validateSegment)

-- | The media-segments API (docs/todo.org section 6, milestone 6h):
-- @GET/POST /items/:id/MediaSegments@ and @DELETE
-- /items/:id/MediaSegments/:segId@. Segments are stored manually
-- (auto-detection is out of scope). All routes enforce folder/policy
-- access on the item via 'getItemWithAccess'; bad bodies or invalid
-- segments are 400, unknown items/segments 404.
segmentsRoutes :: [Route]
segmentsRoutes =
  [ get    [Lit "items", Capture "id", Lit "MediaSegments"] listSegmentsHandler
  , post   [Lit "items", Capture "id", Lit "MediaSegments"] createSegmentHandler
  , delete [Lit "items", Capture "id", Lit "MediaSegments", Capture "segId"] deleteSegmentHandler
  ]

-- | Body of @POST /items/:id/MediaSegments@:
-- @{\"Type\": \"Intro\", \"StartTicks\": n, \"EndTicks\": m, \"Id\"?: \"...\"}@.
data CreateSegmentRequest = CreateSegmentRequest
  { csrId         :: Maybe Text
  , csrType       :: Text
  , csrStartTicks :: Integer
  , csrEndTicks   :: Integer
  }

instance FromJSON CreateSegmentRequest where
  parseJSON = withObject "CreateSegmentRequest" $ \o ->
    CreateSegmentRequest
      <$> o .:? "Id"
      <*> o .:  "Type"
      <*> o .:? "StartTicks" .!= 0
      <*> o .:? "EndTicks" .!= 0

-- Handlers ------------------------------------------------------------------

-- | @GET \/items\/:id\/MediaSegments@ — 404 unknown item, otherwise 200
-- with the item's segment array (empty when none).
listSegmentsHandler :: [(Text, Text)] -> Request -> AppM Response
listSegmentsHandler captures req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just i  -> do
      mItem <- getItemWithAccess req i
      case mItem of
        Nothing -> throwError (NotFound ("no library item with id " <> i))
        Just _  -> jsonResponse <$> listSegments i

-- | @POST \/items\/:id\/MediaSegments@ — validate the body ('validateSegment',
-- 400 on invalid), mint a 16-hex id when absent, upsert, 200 the segment.
createSegmentHandler :: [(Text, Text)] -> Request -> AppM Response
createSegmentHandler captures req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just i  -> do
      mItem <- getItemWithAccess req i
      case mItem of
        Nothing -> throwError (NotFound ("no library item with id " <> i))
        Just _ -> do
          body <- liftIO (strictRequestBody req)
          csr <- case eitherDecode body of
            Left err -> throwError (BadRequest ("invalid JSON body: " <> T.pack err))
            Right v  -> pure v
          sid <- case csrId csr of
            Just s | not (T.null s) -> pure s
            _                       -> liftIO newSegmentId
          let seg = MediaSegment sid i (csrType csr) (csrStartTicks csr) (csrEndTicks csr)
          case validateSegment seg of
            Left e   -> throwError (BadRequest e)
            Right s  -> jsonResponse <$> upsertSegment s

-- | @DELETE \/items\/:id\/MediaSegments\/:segId@ — 200 @{\"status\":\"ok\"}@
-- or 404 when the item/segment does not exist.
deleteSegmentHandler :: [(Text, Text)] -> Request -> AppM Response
deleteSegmentHandler captures req =
  case (lookup "id" captures, lookup "segId" captures) of
    (Nothing, _) -> throwError (BadRequest "missing id capture")
    (_, Nothing) -> throwError (BadRequest "missing segId capture")
    (Just i, Just sid) -> do
      mItem <- getItemWithAccess req i
      case mItem of
        Nothing -> throwError (NotFound ("no library item with id " <> i))
        Just _  -> do
          ok <- deleteSegment i sid
          if ok
            then pure (jsonResponse (object ["status" .= ("ok" :: Text)]))
            else throwError (NotFound ("no segment " <> sid <> " for item " <> i))
