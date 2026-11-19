module FMS.Modules.Stats.Routes
  ( progressHandler
  , continueWatchingHandler
  , popularHandler
  , newHandler
  , ProgressRequest (..)
  ) where

import Control.Applicative ((<|>))
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), decode, object, withObject, (.:), (.=))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import Network.Wai (Request, Response, strictRequestBody)

import FMS.Core.Module (jsonResponse)
import FMS.Core.Monad (AppError (..), AppM)
import FMS.Core.Types.MediaItem (MediaItem (..), simpleItemJson)
import FMS.Core.Types.Progress (simpleProgressEntry)
import FMS.Core.Query (withLegacyItem)
import FMS.Modules.Library (MonadLibrary (..), metaMapNow)
import FMS.Modules.Stats.Monad (MonadStats (..), continueWatching, popular)

-- | Body of @POST /progress@: which item, and where playback is.
data ProgressRequest = ProgressRequest
  { prItemId      :: Text
  , prCurrentTime :: Double
  , prDuration    :: Double
  }

instance FromJSON ProgressRequest where
  parseJSON = withObject "ProgressRequest" $ \o ->
    ProgressRequest
      <$> (o .: "id" <|> o .: "itemId" <|> o .: "ItemId")          -- index.html sends "id"
      <*> (o .: "currentTime" <|> o .: "CurrentTime")
      <*> (o .: "duration" <|> o .: "Duration")

-- | @POST /progress@ with @{\"itemId\": \"...\", \"currentTime\": 12.5,
-- \"duration\": 100.0}@ — records the playback position for the item.
-- 400 on a malformed body, 404 when the item is not in the library,
-- otherwise @{\"status\":\"ok\"}@.
progressHandler :: Request -> AppM Response
progressHandler req = do
  body <- liftIO (strictRequestBody req)
  case decode body of
    Nothing -> throwError (BadRequest "invalid progress payload")
    Just pr -> do
      mItem <- getItem (prItemId pr)
      case mItem of
        Nothing -> throwError (NotFound ("no library item with id " <> prItemId pr))
        Just _ -> do
          recordProgress (prItemId pr) (prCurrentTime pr) (prDuration pr)
          pure (jsonResponse (object ["status" .= ("ok" :: Text)]))

-- | @GET /continuewatching@ — items with unfinished progress, joined
-- with their library entries, most recently reported first. LEGACY
-- simple lowercase shape for the stamno-media UI:
-- @[{\"id\",\"title\",\"size\",\"mime\",\"progress\": 0..100}, ...]@
-- ('simpleProgressEntry').
continueWatchingHandler :: AppM Response
continueWatchingHandler = do
  allStats <- getAllStats
  byId <- indexById
  let entries =
        [ (item, p)
        | (ident, p) <- continueWatching allStats
        , Just item <- [HM.lookup ident byId]
        ]
  metaMap <- metaMapNow   -- m40/m43
  pure (jsonResponse [ withLegacyItem metaMap item (simpleProgressEntry item p) | (item, p) <- entries ])

-- | @GET /popular@ — items played at least once, most-played first
-- (top 20), in the legacy simple lowercase shape ('simpleItemJson').
popularHandler :: AppM Response
popularHandler = do
  allStats <- getAllStats
  byId <- indexById
  let ranked =
        [ item
        | (ident, _s) <- take 20 (popular allStats)
        , Just item <- [HM.lookup ident byId]
        ]
  metaMap <- metaMapNow   -- m40/m43
  pure (jsonResponse (map (\i -> withLegacyItem metaMap i (simpleItemJson i)) ranked))

-- | @GET /new@ — the most recently added library items (by file mtime),
-- newest first, top 20, in the legacy simple lowercase shape.
newHandler :: AppM Response
newHandler = do
  items <- listItems
  metaMap <- metaMapNow   -- m40/m43
  pure (jsonResponse (map (\i -> withLegacyItem metaMap i (simpleItemJson i)) (take 20 (sortOn (Down . itemAddedAt) items))))

-- | The library items keyed by id, for joining stats against titles.
indexById :: MonadLibrary m => m (HashMap Text MediaItem)
indexById = do
  items <- listItems
  pure (HM.fromList [(itemId i, i) | i <- items])
