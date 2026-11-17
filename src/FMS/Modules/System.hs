module FMS.Modules.System
  ( systemModule
  ) where

import Control.Applicative ((<|>))
import Control.Concurrent (MVar, forkIO, putMVar, threadDelay)
import Control.Concurrent.STM (TVar, atomically, readTVarIO, writeTVar)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (FromJSON (..), Value, eitherDecode, object, withObject, (.:?), (.=))
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Network.HTTP.Types (status200)
import Network.Wai (Request, Response, queryString, responseLBS, strictRequestBody)
import Numeric (showHex)
import System.Directory (doesFileExist, getFileSize, getModificationTime)
import System.FilePath ((</>))
import Text.Read (readMaybe)

import FMS (version)
import FMS.Core.Module (Module (..), Segment (..), get, jsonResponse, post)
import FMS.Core.Monad (AppError (..), AppM, MonadLog (..), envConfig, getState, putState)
import FMS.Core.Query (paginate, queryParams)
import FMS.Core.Types.Config (Config (..))
import FMS.Core.Types.Log (LogEntry (..))
import FMS.Core.Types.User (User (..))
import FMS.Core.Types.ServerConfig
  ( ServerConfig (..), ServerConfigPatch (..), applyServerConfigPatch
  , saveServerConfig, serverConfigFilePath
  )
-- FMS.Modules.Auth.Monad imports only Core/Auth/Library.Monad, so importing
-- it here does not create a cycle (only FMS.Server imports this module).
import FMS.Modules.Auth.Monad (MonadAuth (..))

-- | The system module: server info, health check, configuration and
-- shutdown/restart (docs/todo.org section 1, milestone 1b). Init only
-- registers the stable server id; the live 'ServerConfig' TVar and the
-- shutdown 'MVar' are registered by "FMS.Server" itself.
systemModule :: Module
systemModule = Module
  { moduleName = "system"
  , moduleInit = do
      cfg <- asks envConfig
      putState "system.id" (serverIdFor cfg)
  , moduleRoutes =
      [ get  [Lit "System", Lit "Info", Lit "Public"] infoPublic
      , get  [Lit "System", Lit "Info"] infoHandler
      , get  [Lit "System", Lit "Configuration"] configHandler
      , post [Lit "System", Lit "Configuration"] updateConfigHandler
      , post [Lit "System", Lit "Shutdown"] (shutdownLikeHandler "shutdown" "shutting down")
      , post [Lit "System", Lit "Restart"]  (shutdownLikeHandler "restart" "restarting")
      , post [Lit "System", Lit "Wizard", Lit "Startup"]   wizardStartupHandler
      , post [Lit "System", Lit "Wizard", Lit "Completed"] wizardCompletedHandler
      , get  [Lit "Branding", Lit "Configuration"] brandingGetHandler
      , post [Lit "Branding", Lit "Configuration"] brandingPostHandler
      , get  [Lit "System", Lit "Logs"] logsListHandler
      , get  [Lit "System", Lit "Logs", Lit "Log"] logsLogHandler
      , get  [Lit "healthz"] healthz
      ]
  }

-- | @GET /System/Info/Public@ — public server info, Jellyfin-style. The
-- ServerName is read live from the server-config TVar, so a
-- @POST /System/Configuration@ change is visible immediately (and is
-- persisted in @stateDir/config.json@).
infoPublic :: [(Text, Text)] -> Request -> AppM Response
infoPublic _captures _req = do
  serverId <- fromMaybe "unknown" <$> (getState "system.id" :: AppM (Maybe Text))
  sc <- currentServerConfig
  cfg <- asks envConfig
  let tlsOn = isJust (configTlsCert cfg) && isJust (configTlsKey cfg)  -- milestone 13d
  pure (jsonResponse (object
    [ "ServerName" .= scServerName sc
    , "Version"    .= version
    , "Id"         .= serverId
    , "StartupWizardCompleted" .= scStartupCompleted sc  -- milestone 10b
    , "SupportsHttps" .= tlsOn
    , "HttpsPort"     .= (if tlsOn then Just (scPort sc) else Nothing)
    ]))

-- | @GET /System/Info@ — full server info (Jellyfin-shaped): id, name,
-- version, library path, port, the live require-auth flag, and the loaded
-- module names. Always 200.
infoHandler :: [(Text, Text)] -> Request -> AppM Response
infoHandler _captures _req = do
  serverId <- fromMaybe "unknown" <$> (getState "system.id" :: AppM (Maybe Text))
  sc   <- currentServerConfig
  mods <- fromMaybe [] <$> (getState "system.moduleNames" :: AppM (Maybe [Text]))
  cfg <- asks envConfig
  let tlsOn = isJust (configTlsCert cfg) && isJust (configTlsKey cfg)  -- milestone 13d
  pure (jsonResponse (object
    [ "Id"          .= serverId
    , "ServerName"  .= scServerName sc
    , "Version"     .= version
    , "LibraryPath" .= T.pack (scLibraryPath sc)
    , "Port"        .= scPort sc
    , "RequireAuth" .= scRequireAuth sc
    , "Modules"     .= mods
    , "StartupWizardCompleted" .= scStartupCompleted sc  -- milestone 10b
    , "SupportsHttps" .= tlsOn
    , "HttpsPort"     .= (if tlsOn then Just (scPort sc) else Nothing)
    ]))

-- | @GET /System/Configuration@ — 200 the current 'ServerConfig' JSON.
configHandler :: [(Text, Text)] -> Request -> AppM Response
configHandler _captures _req =
  jsonResponse <$> currentServerConfig

-- | @POST /System/Configuration@ — apply a partial patch
-- (@ServerConfigPatch@: every field optional) over the current config,
-- persist it to @stateDir/config.json@, update the live TVar (so
-- ServerName and RequireAuth take effect immediately), and answer 200
-- with the new config. Bad JSON -> 400. Note: Port and LibraryPath only
-- take full effect on the next start/restart (warp is already bound, the
-- library already scanned).
updateConfigHandler :: [(Text, Text)] -> Request -> AppM Response
updateConfigHandler _captures req = do
  body <- liftIO (strictRequestBody req)
  case eitherDecode body of
    Left err -> throwError (BadRequest ("invalid JSON body: " <> T.pack err))
    Right patch -> do
      sc <- currentServerConfig
      let sc' = applyServerConfigPatch sc patch
      cfg <- asks envConfig
      liftIO (saveServerConfig (serverConfigFilePath (configStateDir cfg)) sc')
      mTv <- getState "system.serverConfig" :: AppM (Maybe (TVar ServerConfig))
      case mTv of
        Just tv -> liftIO (atomically (writeTVar tv sc'))
        Nothing -> pure ()
      logInfo ("system: configuration updated (ServerName=" <> scServerName sc'
               <> ", Port=" <> T.pack (show (scPort sc')) <> ")")
      pure (jsonResponse sc')

-- | Body of @POST /System/Wizard/Startup@ (milestone 10b): the admin
-- account name and password; @Name@ or @Username@ both accepted, both
-- optional (defaults "admin" / "admin").
data WizardStartupRequest = WizardStartupRequest
  { wsrName :: Maybe Text
  , wsrPw   :: Maybe Text
  }

instance FromJSON WizardStartupRequest where
  parseJSON = withObject "WizardStartupRequest" $ \o ->
    WizardStartupRequest
      <$> (o .:? "Name" <|> o .:? "Username")
      <*> o .:? "Password"

-- | @POST /System/Wizard/Startup@ — first-run wizard (milestone 10b,
-- open path so it works in strict mode before any user exists). When NO
-- user exists yet, creates the admin user and marks the wizard completed;
-- when users already exist -> 400. Bad JSON -> 400. 200
-- @{"status":"ok","userId":"..."}@.
wizardStartupHandler :: [(Text, Text)] -> Request -> AppM Response
wizardStartupHandler _captures req = do
  body <- liftIO (strictRequestBody req)
  case eitherDecode body of
    Left err -> throwError (BadRequest ("invalid JSON body: " <> T.pack err))
    Right wsr -> do
      users <- listUsers
      case users of
        (_ : _) -> throwError (BadRequest "startup wizard already completed")
        [] -> do
          let name = fromMaybe "admin" (wsrName wsr)
              pw   = fromMaybe "admin" (wsrPw wsr)
          u <- createUser name pw True
          sc <- currentServerConfig
          let sc' = sc { scStartupCompleted = True }
          persistServerConfig sc'
          logInfo ("system: startup wizard completed, created admin user " <> userName u)
          pure (jsonResponse (object
            [ "status" .= ("ok" :: Text)
            , "userId" .= userId u
            ]))

-- | @POST /System/Wizard/Completed@ — mark the startup wizard completed
-- without creating a user (milestone 10b); 200 @{"status":"ok"}@.
wizardCompletedHandler :: [(Text, Text)] -> Request -> AppM Response
wizardCompletedHandler _captures _req = do
  sc <- currentServerConfig
  let sc' = sc { scStartupCompleted = True }
  persistServerConfig sc'
  pure (jsonResponse (object ["status" .= ("ok" :: Text)]))

-- | @GET /Branding/Configuration@ — 200 the branding fields of the server
-- config (milestone 10b, open path like Jellyfin's public branding).
brandingGetHandler :: [(Text, Text)] -> Request -> AppM Response
brandingGetHandler _captures _req = do
  sc <- currentServerConfig
  pure (jsonResponse (brandingObject sc))

-- | @POST /Branding/Configuration@ — partial patch over the branding
-- fields (@LoginDisclaimer@?, @CustomCss@?), persisted to config.json and
-- applied to the live TVar; 200 with the updated branding object.
-- Reuses 'ServerConfigPatch' (its 10b fields are exactly the branding
-- ones); 400 on bad JSON.
brandingPostHandler :: [(Text, Text)] -> Request -> AppM Response
brandingPostHandler _captures req = do
  body <- liftIO (strictRequestBody req)
  case eitherDecode body of
    Left err -> throwError (BadRequest ("invalid JSON body: " <> T.pack err))
    Right patch -> do
      sc <- currentServerConfig
      let sc' = applyServerConfigPatch sc (patch :: ServerConfigPatch)
      persistServerConfig sc'
      pure (jsonResponse (brandingObject sc'))

-- | The Jellyfin-shaped branding response object.
brandingObject :: ServerConfig -> Value
brandingObject sc = object
  [ "LoginDisclaimer" .= scLoginDisclaimer sc
  , "CustomCss"       .= scCustomCss sc
  , "SplashscreenEnabled" .= False
  ]

-- | Persist a new server config to @stateDir/config.json@ AND update the
-- live "system.serverConfig" TVar (milestone 10b helper, same steps as
-- 'updateConfigHandler').
persistServerConfig :: ServerConfig -> AppM ()
persistServerConfig sc' = do
  cfg <- asks envConfig
  liftIO (saveServerConfig (serverConfigFilePath (configStateDir cfg)) sc')
  mTv <- getState "system.serverConfig" :: AppM (Maybe (TVar ServerConfig))
  case mTv of
    Just tv -> liftIO (atomically (writeTVar tv sc'))
    Nothing -> pure ()

-- | @POST /System/Shutdown@ and @POST /System/Restart@ — answer 200
-- @{"status": ...}@ first, then fire the shutdown 'MVar' with the tag
-- ("shutdown" / "restart"); the main thread in "FMS.Server" performs the
-- graceful cleanup. Warp's graceful shutdown lets this very response
-- the MVar is filled from a short-delayed thread, so the response is
-- constructed, returned, and flushed by warp BEFORE teardown starts.
shutdownLikeHandler
  :: Text -> Text -> [(Text, Text)] -> Request -> AppM Response
shutdownLikeHandler tag statusMsg _captures _req = do
  let resp = jsonResponse (object ["status" .= statusMsg])
  mMv <- getState "system.shutdown" :: AppM (Maybe (MVar Text))
  case mMv of
    Just mv -> do
      _ <- liftIO (forkIO (threadDelay 500000 >> putMVar mv tag))
      pure ()
    Nothing -> pure ()
  pure resp

-- | @GET /System/Logs@ (milestone 11b) — 200 the Jellyfin-shaped log-file
-- list. The server's log is reported as @server.log@; its size/mtime are
-- taken from @stateDir/activity.log@ when present (0 / now otherwise).
logsListHandler :: [(Text, Text)] -> Request -> AppM Response
logsListHandler _captures _req = do
  cfg <- asks envConfig
  let logPath = configStateDir cfg </> "activity.log"
  exists <- liftIO (doesFileExist logPath)
  (sizeBytes, mtime) <- if exists
    then do
      sz  <- liftIO (getFileSize logPath)
      mt  <- liftIO (getModificationTime logPath)
      pure (sz, mt)
    else do
      now <- liftIO getCurrentTime
      pure (0, now)
  pure (jsonResponse [object
    [ "Name"         .= ("server.log" :: Text)
    , "SizeBytes"    .= sizeBytes
    , "DateModified" .= mtime
    ]])

-- | @GET /System/Logs/Log?startIndex=&limit=@ (milestone 11b) — 200
-- @{\"Items\": [LogEntry...], \"TotalRecordCount\": n}@, NEWEST FIRST,
-- paginated. @startIndex@ defaults to 0, @limit@ to 100; invalid values
-- fall back to the defaults. The entries come from the "system.logbuf"
-- ring TVar kept by the 'MonadLog' instance.
logsLogHandler :: [(Text, Text)] -> Request -> AppM Response
logsLogHandler _captures req = do
  mTv <- getState "system.logbuf" :: AppM (Maybe (TVar [LogEntry]))
  buf <- maybe (pure []) (liftIO . readTVarIO) mTv
  let qparams = queryParams (queryString req)
      startIndex = fromMaybe (0 :: Int) (lookup "startIndex" qparams >>= readMaybe . T.unpack)
      limit      = fromMaybe (100 :: Int) (lookup "limit" qparams >>= readMaybe . T.unpack)
      newestFirst = reverse buf
      total = length newestFirst
      page = paginate (Just (max 0 startIndex)) (Just limit) newestFirst
  pure (jsonResponse (object
    [ "Items" .= page
    , "TotalRecordCount" .= total
    ]))

-- | @GET /healthz@ — liveness probe.
healthz :: [(Text, Text)] -> Request -> AppM Response
healthz _captures _req =
  pure (responseLBS status200 [("Content-Type", "text/plain; charset=utf-8")] "ok")

-- | The live server config from the typed registry, falling back to the
-- static env config when the TVar is absent (e.g. in tests).
currentServerConfig :: AppM ServerConfig
currentServerConfig = do
  mTv <- getState "system.serverConfig" :: AppM (Maybe (TVar ServerConfig))
  case mTv of
    Just tv -> liftIO (readTVarIO tv)
    Nothing -> do
      cfg <- asks envConfig
      pure ServerConfig
        { scServerName  = configServerName cfg
        , scRequireAuth = configRequireAuth cfg
        , scLibraryPath = configLibraryPath cfg
        , scPort        = configPort cfg
        , scStartupCompleted = False
        , scLoginDisclaimer  = ""
        , scCustomCss        = ""
        }

-- | A stable-ish server id derived from the config (library path + port),
-- rendered as a 32-char hex string like Jellyfin's device id.
serverIdFor :: Config -> Text
serverIdFor cfg =
  let h = hashText (T.pack (configLibraryPath cfg) <> ":" <> T.pack (show (configPort cfg)))
      hex = showHex h ""
  in T.pack (replicate (max 0 (32 - length hex)) '0' ++ hex)

hashText :: Text -> Integer
hashText = T.foldl' (\acc c -> acc * 31 + toInteger (fromEnum c)) 5381
