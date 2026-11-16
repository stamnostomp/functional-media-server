module FMS.Modules.Devices.Routes
  ( devicesRoutes
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), eitherDecode, object, withObject, (.:), (.:?), (.!=), (.=))
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.Wai (Request, Response, queryString, strictRequestBody)

import FMS.Core.Module (Route, Segment (..), delete, get, jsonResponse, post)
import FMS.Core.Monad (AppError (..), AppM)
import FMS.Core.Query (queryParams)
import FMS.Modules.Devices.Monad (MonadDevices (..))
import FMS.Modules.Devices.Types (Device (..))

-- | The client-device registry API (milestone 10c, docs/todo.org section
-- 8, Jellyfin =/Devices= shape). Behind the normal auth gate (in strict
-- mode a valid token is required); no extra restriction. Bad bodies are
-- 400, unknown device ids are 404.
devicesRoutes :: [Route]
devicesRoutes =
  [ get    [Lit "Devices"] listDevicesHandler
  , post   [Lit "Devices"] upsertDeviceHandler
  , get    [Lit "Devices", Capture "id"] getDeviceHandler
  , delete [Lit "Devices", Capture "id"] deleteDeviceHandler
  , post   [Lit "Devices", Capture "id", Lit "Capabilities"] capabilitiesHandler
  ]

-- | Body of @POST /Devices@ (self-report, Jellyfin-shaped):
-- @{\"DeviceId\"?: \"...\", \"Name\": \"...\", \"AppName\"?, \"AppVersion\"?,
--   \"Capabilities\"?, \"LastUserId\"?}@. An absent/empty DeviceId is
-- minted server-side.
data DeviceRequest = DeviceRequest
  { drDeviceId   :: Maybe Text
  , drName       :: Text
  , drAppName    :: Text
  , drAppVersion :: Text
  , drCapabilities :: [Text]
  , drLastUserId :: Maybe Text
  }

instance FromJSON DeviceRequest where
  parseJSON = withObject "DeviceRequest" $ \o ->
    DeviceRequest
      <$> o .:? "DeviceId"
      <*> o .:  "Name"
      <*> o .:? "AppName"      .!= ""
      <*> o .:? "AppVersion"   .!= ""
      <*> o .:? "Capabilities" .!= []
      <*> o .:? "LastUserId"

-- | Body of @POST /Devices/:id/Capabilities@:
-- @{\"PlayableMediaTypes\"?: [...], \"SupportedCommands\"?: [...]}@ —
-- both are merged (nub) into the device's capability list.
data CapabilitiesRequest = CapabilitiesRequest
  { crPlayableMediaTypes :: [Text]
  , crSupportedCommands  :: [Text]
  }

instance FromJSON CapabilitiesRequest where
  parseJSON = withObject "CapabilitiesRequest" $ \o ->
    CapabilitiesRequest
      <$> o .:? "PlayableMediaTypes" .!= []
      <*> o .:? "SupportedCommands"  .!= []

-- Handlers ------------------------------------------------------------------

-- | @GET /Devices@ — 200 the Jellyfin envelope
-- @{\"Items\": [...], \"TotalRecordCount\": n}@; the optional @deviceId@
-- query param filters to a single device.
listDevicesHandler :: [(Text, Text)] -> Request -> AppM Response
listDevicesHandler _captures req = do
  devices <- listDevices
  let qparams = queryParams (queryString req)
      filtered = case lookup "deviceId" qparams of
        Just did -> filter (\d -> devId d == did) devices
        Nothing  -> devices
  pure (jsonResponse (object
    [ "Items" .= filtered
    , "TotalRecordCount" .= length filtered
    ]))

-- | @POST /Devices@ — register or update a client device (upsert). Mints
-- an id when @DeviceId@ is absent/empty, stamps @LastSeenAt@. 400 on a
-- malformed body or an empty @Name@; otherwise 200 with the device JSON.
upsertDeviceHandler :: [(Text, Text)] -> Request -> AppM Response
upsertDeviceHandler _captures req = do
  body <- liftIO (strictRequestBody req)
  case eitherDecode body of
    Left err -> throwError (BadRequest ("invalid JSON body: " <> T.pack err))
    Right dr
      | T.null (T.strip (drName dr)) ->
          throwError (BadRequest "device name must not be empty")
      | otherwise -> do
          d <- upsertDeviceM (Device
            { devId           = fromMaybe "" (drDeviceId dr)
            , devName         = drName dr
            , devAppName      = drAppName dr
            , devAppVersion   = drAppVersion dr
            , devCapabilities = drCapabilities dr
            , devLastUserId   = drLastUserId dr
            , devLastSeenAt   = Nothing   -- stamped by upsertDeviceM
            })
          pure (jsonResponse d)

-- | @GET /Devices/:id@ — 200 the device JSON, 404 when unknown.
getDeviceHandler :: [(Text, Text)] -> Request -> AppM Response
getDeviceHandler captures _req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just did -> do
      mD <- getDevice did
      case mD of
        Nothing -> throwError (NotFound ("no device with id " <> did))
        Just d  -> pure (jsonResponse d)

-- | @DELETE /Devices/:id@ — 200 @{\"status\":\"ok\"@}, 404 when unknown.
deleteDeviceHandler :: [(Text, Text)] -> Request -> AppM Response
deleteDeviceHandler captures _req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just did -> do
      ok <- deleteDevice did
      if ok
        then pure (jsonResponse (object ["status" .= ("ok" :: Text)]))
        else throwError (NotFound ("no device with id " <> did))

-- | @POST /Devices/:id/Capabilities@ — merge @PlayableMediaTypes@ +
-- @SupportedCommands@ (nub, order kept) into the device's capabilities.
-- 400 on a malformed body, 404 when the device is unknown; otherwise 200
-- with the updated device JSON.
capabilitiesHandler :: [(Text, Text)] -> Request -> AppM Response
capabilitiesHandler captures req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just did -> do
      mD <- getDevice did
      case mD of
        Nothing -> throwError (NotFound ("no device with id " <> did))
        Just d -> do
          body <- liftIO (strictRequestBody req)
          case eitherDecode body of
            Left err -> throwError (BadRequest ("invalid JSON body: " <> T.pack err))
            Right cr -> do
              let merged = nub (devCapabilities d ++ crPlayableMediaTypes cr ++ crSupportedCommands cr)
              d' <- upsertDeviceM (d { devCapabilities = merged })
              pure (jsonResponse d')
