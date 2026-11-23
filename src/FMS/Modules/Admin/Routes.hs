module FMS.Modules.Admin.Routes
  ( adminRoutes
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), ToJSON (..), Value(..), eitherDecode, object, withObject, (.:), (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Network.Wai (Request, Response, strictRequestBody)

import FMS (version)
import FMS.Core.Memory (currentRssOrZero)
import FMS.Core.Module (Route, Segment (..), get, jsonResponse, post)
import FMS.Core.Monad (AppError (..), AppM, getState)
import FMS.Modules.Admin.Monad (MonadAdmin (..))
import FMS.Modules.Admin.Types (ScheduledTask (..), countsByType, uptimeSeconds)
import FMS.Modules.Auth.Monad (requireAdmin)
import FMS.Modules.Devices.Monad (MonadDevices (..))
import FMS.Modules.Library.Monad (MonadLibrary (..))
import FMS.Modules.Sessions.Monad (MonadSessions (..))
import FMS.Modules.System (currentServerConfig)

-- | The admin routes (docs/todo.org section 11, milestone 11a),
-- Jellyfin-shaped: @GET /System/ActivityLog@ and @GET/POST /ScheduledTasks...@,
-- plus (milestone 10d) the @GET /Dashboard@ aggregate for the stamno-media
-- UI.
adminRoutes :: [Route]
adminRoutes =
  [ get  [Lit "Dashboard"]                                         dashboardHandler
  , get  [Lit "System", Lit "ActivityLog"]                        activityLogHandler
  , get  [Lit "ScheduledTasks"]                                    listTasksHandler
  , post [Lit "ScheduledTasks", Capture "id", Lit "Run"]           runTaskHandler
  , post [Lit "ScheduledTasks", Capture "id", Lit "Enable"]        enableTaskHandler
  ]

-- | @GET /Dashboard@ — one aggregate JSON object (milestone 10d,
-- docs/todo.org section 12) for the stamno-media UI:
-- @Server@ (current config + @UptimeSeconds@), @Library@
-- (@TotalItems@ + @Counts@ per type), @Sessions@, @Devices@,
-- @Activity@ (newest 20), @ScheduledTasks@. Admin-only (milestone 16).
dashboardHandler :: [(Text, Text)] -> Request -> AppM Response
dashboardHandler _captures req = do
  requireAdmin req  -- milestone 16
  sc <- currentServerConfig
  now <- liftIO getCurrentTime
  mStarted <- getState "system.startedAt" :: AppM (Maybe UTCTime)
  let uptime = maybe 0 (uptimeSeconds now) mStarted
      serverJson = case toJSON sc of
        Object o -> Object (KeyMap.insert (Key.fromText "UptimeSeconds") (toJSON uptime)
                            (KeyMap.insert (Key.fromText "Version") (toJSON version) o))  -- m90
        v        -> v
  -- memory monitoring (todo section 15): current RSS (0 when unavailable)
  rss <- currentRssOrZero
  let serverJson' = case serverJson of
        Object o -> Object (KeyMap.insert (Key.fromText "MemoryUsageBytes") (toJSON rss) o)
        v        -> v
  items    <- listItems
  sessions <- listSessions
  devices  <- listDevices
  activity <- listActivity
  tasks    <- listTasks
  pure (jsonResponse (object
    [ "Server"         .= serverJson'
    , "Library"        .= object
        [ "TotalItems" .= length items
        , "Counts"     .= countsByType items
        ]
    , "Sessions"       .= sessions
    , "Devices"        .= devices
    , "Activity"       .= take 20 activity
    , "ScheduledTasks" .= tasks
    ]))

-- | @GET /System/ActivityLog@ — 200 @{\"Items\": [...], \"TotalRecordCount\": n}@,
-- newest entries first.
activityLogHandler :: [(Text, Text)] -> Request -> AppM Response
activityLogHandler _captures req = do
  requireAdmin req  -- milestone 16
  entries <- listActivity
  pure (jsonResponse (object
    [ "Items" .= entries
    , "TotalRecordCount" .= length entries
    ]))

-- | @GET /ScheduledTasks@ — 200 JSON array of the registered tasks
-- (sorted by id), including LastRun/LastResult of the most recent run.
listTasksHandler :: [(Text, Text)] -> Request -> AppM Response
listTasksHandler _captures req = do
  requireAdmin req  -- milestone 16
  jsonResponse <$> listTasks

-- | @POST /ScheduledTasks/:id/Run@ — run the task now; 200 with the
-- updated task JSON (LastRun + LastResult set), 404 for an unknown id.
runTaskHandler :: [(Text, Text)] -> Request -> AppM Response
runTaskHandler captures req = do
  requireAdmin req  -- milestone 16
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
  requireAdmin req  -- milestone 16
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
