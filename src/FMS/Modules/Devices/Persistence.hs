module FMS.Modules.Devices.Persistence
  ( loadDevices
  , saveDevices
  ) where

import Control.Exception (SomeException, try)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import System.Directory (doesFileExist)

import FMS.Modules.Devices.Types (Device)

-- | Load the persisted device store (@stateDir/devices.json@): a bare
-- JSON object mapping device id -> 'Device'. A missing or unreadable file
-- yields an empty map — best-effort, never fatal. Reads the file strictly
-- ('BS.readFile', like 'FMS.Modules.Library.Folders.loadFolders').
loadDevices :: FilePath -> IO (HashMap Text Device)
loadDevices path = do
  exists <- doesFileExist path
  if not exists
    then pure HM.empty
    else do
      result <- try (BS.readFile path) :: IO (Either SomeException BS.ByteString)
      pure $ case result of
        Left _      -> HM.empty
        Right bytes -> fromMaybe HM.empty (Aeson.decode (LBS.fromStrict bytes))

-- | Persist the whole device map as a bare JSON object.
saveDevices :: FilePath -> HashMap Text Device -> IO ()
saveDevices path devices = LBS.writeFile path (Aeson.encode devices)
