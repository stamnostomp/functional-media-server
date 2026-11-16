module FMS.Modules.Admin.Routes
  ( adminRoutes
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), eitherDecode, object, withObject, (.:), (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Network.Wai (Request, Response, strictRequestBody)

import FMS.Core.Module (Route, Segment (..), get, jsonResponse, post)
import FMS.Core.Monad (AppError (..), AppM)
import FMS.Modules.Admin.Monad (MonadAdmin (..))
import FMS.Modules.Admin.Types (ScheduledTask (..))

-- | The admin routes (docs/todo.org section 11, milestone 11a),
-- Jellyfin-shaped: @GET /System/ActivityLog@ and @GET/POST /ScheduledTasks...@.
adminRoutes :: [Route]
adminRoutes =
  [ get  [Lit "System", Lit "ActivityLog"]                        activityLogHandler
  , get  [Lit "ScheduledTasks"]                                    listTasksHandler
  , post [Lit "ScheduledTasks", Capture "id", Lit "Run"]           runTaskHandler
  , post [Lit "ScheduledTasks", Capture "id", Lit "Enable"]        enableTaskHandler
  ]

-- | @GET /System/ActivityLog@ — 200 @{\"Items\": [...], \"TotalRecordCount\": n}@,
-- newest entries first.
activityLogHandler :: [(Text, Text)] -> Request -> AppM Response
activityLogHandler _captures _req = do
  entries <- listActivity
  pure (jsonResponse (object
    [ "Items" .= entries
    , "TotalRecordCount" .= length entries
    ]))

-- | @GET /ScheduledTasks@ — 200 JSON array of the registered tasks
-- (sorted by id), including LastRun/LastResult of the most recent run.
listTasksHandler :: [(Text, Text)] -> Request -> AppM Response
listTasksHandler _captures _req =
  jsonResponse <$> listTasks

-- | @POST /ScheduledTasks/:id/Run@ — run the task now; 200 with the
-- updated task JSON (LastRun + LastResult set), 404 for an unknown id.
runTaskHandler :: [(Text, Text)] -> Request -> AppM Response
runTaskHandler captures _req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just tid -> do
      mT <- runTask tid
      case mT of
        Nothing -> throwError (NotFound ("no scheduled task with id " <> tid))
        Just t  -> pure (jsonResponse t)

-- | Body of @POST /ScheduledTasks/:id/Enable@: @{\"Enabled\": true|false}@.
data EnableRequest = EnableRequest
  { enEnabled :: Bool
  }

instance FromJSON EnableRequest where
  parseJSON = withObject "EnableRequest" $ \o ->
    EnableRequest <$> o .: "Enabled"

-- | @POST /ScheduledTasks/:id/Enable@ — enable/disable a task; 200 with
-- the updated task JSON, 404 for an unknown id, 400 on a malformed body.
enableTaskHandler :: [(Text, Text)] -> Request -> AppM Response
enableTaskHandler captures req = do
  body <- liftIO (strictRequestBody req)
  case eitherDecode body of
    Left err -> throwError (BadRequest ("invalid JSON body: " <> T.pack err))
    Right er ->
      case lookup "id" captures of
        Nothing -> throwError (BadRequest "missing id capture")
        Just tid -> do
          ok <- enableTask tid (enEnabled er)
          if not ok
            then throwError (NotFound ("no scheduled task with id " <> tid))
            else do
              tasks <- listTasks
              case [ t | t <- tasks, stId t == tid ] of
                (t : _) -> pure (jsonResponse t)
                []      -> throwError (NotFound ("no scheduled task with id " <> tid))
