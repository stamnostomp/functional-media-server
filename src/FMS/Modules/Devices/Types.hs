module FMS.Modules.Devices.Types
  ( Device(..)
  , upsertDevice
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:?), (.!=), (.=))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)

-- | One registered client device (milestone 10c, docs/todo.org section 8,
-- Jellyfin =/Devices= shape), persisted in @stateDir/devices.json@.
data Device = Device
  { devId           :: !Text            -- ^ minted server-side when absent (16 hex)
  , devName         :: !Text            -- ^ human-readable device name (required on create)
  , devAppName      :: !Text            -- ^ client app, e.g. "Jellyfin Web"
  , devAppVersion   :: !Text
  , devCapabilities :: ![Text]          -- ^ merged PlayableMediaTypes + SupportedCommands
  , devLastUserId   :: !(Maybe Text)    -- ^ last user that reported from this device
  , devLastSeenAt   :: !(Maybe UTCTime) -- ^ last upsert time
  } deriving (Eq, Show)

-- | API/persistence JSON, Jellyfin PascalCase keys. Optional fields
-- default on decode so old/partial devices.json files keep loading.
instance ToJSON Device where
  toJSON d = object
    [ "Id"           .= devId d
    , "Name"         .= devName d
    , "AppName"      .= devAppName d
    , "AppVersion"   .= devAppVersion d
    , "Capabilities" .= devCapabilities d
    , "LastUserId"   .= devLastUserId d
    , "LastSeenAt"   .= devLastSeenAt d
    ]

instance FromJSON Device where
  parseJSON = withObject "Device" $ \o ->
    Device
      <$> o .:? "Id"           .!= ""
      <*> o .:? "Name"         .!= ""
      <*> o .:? "AppName"      .!= ""
      <*> o .:? "AppVersion"   .!= ""
      <*> o .:? "Capabilities" .!= []
      <*> o .:? "LastUserId"   .!= Nothing
      <*> o .:? "LastSeenAt"   .!= Nothing

-- | Pure upsert: insert or update the device under its id, preserving the
-- id. A device with an EMPTY id is NOT inserted here — the caller mints
-- one first (see 'FMS.Modules.Devices.Monad.upsertDeviceM').
upsertDevice :: Device -> HashMap Text Device -> HashMap Text Device
upsertDevice d m
  | T.null (devId d) = m   -- caller must mint an id first
  | otherwise        = HM.insert (devId d) d m
