module FMS.Modules.Stats.Routes
  ( progressHandler
  , continueWatchingHandler
  , popularHandler
  , newHandler
  ) where

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
import FMS.Core.Types.MediaItem (MediaItem (..))
import FMS.Core.Types.Progress (ItemStats (..))
import FMS.Modules.Library (MonadLibrary (..))
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
      <$> o .: "itemId"
      <*> o .: "currentTime"
      <*> o .: "duration"

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

-- | @GET /continuewatching@ — items with unfinished progress, joined with
-- their library titles: @[{\"id\", \"title\", \"progress\": {...}}, ...]@,
-- most recently reported first.
continueWatchingHandler :: AppM Response
continueWatchingHandler = do
  allStats <- getAllStats
  byId <- indexById
  let entries =
        [ (item, p)
        | (ident, p) <- continueWatching allStats
        , Just item <- [HM.lookup ident byId]
        ]
  pure (jsonResponse
    [ object
        [ "id"       .= itemId item
        , "title"    .= itemTitle item
        , "progress" .= p
        ]
    | (item, p) <- entries
    ])

-- | @GET /popular@ — items played at least once, most-played first:
-- @[{\"id\", \"title\", \"playCount\", \"lastPlayedAt\"}, ...]@ (top 20).
popularHandler :: AppM Response
popularHandler = do
  allStats <- getAllStats
  byId <- indexById
  let ranked =
        [ (item, s)
        | (ident, s) <- take 20 (popular allStats)
        , Just item <- [HM.lookup ident byId]
        ]
  pure (jsonResponse
    [ object
        [ "id"           .= itemId item
        , "title"        .= itemTitle item
        , "playCount"    .= statsPlayCount s
        , "lastPlayedAt" .= statsLastPlayedAt s
        ]
    | (item, s) <- ranked
    ])

-- | @GET /new@ — the most recently added library items (by file mtime),
-- newest first, top 20.
newHandler :: AppM Response
newHandler = do
  items <- listItems
  pure (jsonResponse (take 20 (sortOn (Down . itemAddedAt) items)))

-- | The library items keyed by id, for joining stats against titles.
indexById :: MonadLibrary m => m (HashMap Text MediaItem)
indexById = do
  items <- listItems
  pure (HM.fromList [(itemId i, i) | i <- items])
