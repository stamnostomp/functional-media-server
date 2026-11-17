module FMS.Modules.UserData.Persistence
  ( UserDataMap
  , loadUserData
  , saveUserData
  , DisplayPrefsMap
  , loadDisplayPrefs
  , saveDisplayPrefs
  ) where

import Control.Exception (SomeException, try)
import qualified Data.Aeson as Aeson
import Data.Aeson (Value)
import qualified Data.ByteString.Lazy as LBS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import System.Directory (doesFileExist)

import FMS.Modules.UserData.Types (UserData)

-- | The whole persisted state: @userId -> itemId -> UserData@, stored as a
-- nested JSON object at @stateDir/userdata.json@.
type UserDataMap = HashMap Text (HashMap Text UserData)

-- | Load the persisted user-data file (@stateDir/userdata.json@): a nested
-- JSON object mapping user id -> item id -> 'UserData'. A missing or
-- unreadable file yields an empty map — user data is best-effort, never
-- fatal.
loadUserData :: FilePath -> IO UserDataMap
loadUserData path = do
  exists <- doesFileExist path
  if not exists
    then pure HM.empty
    else do
      result <- try (LBS.readFile path) :: IO (Either SomeException LBS.ByteString)
      pure $ case result of
        Left _      -> HM.empty
        Right bytes -> fromMaybe HM.empty (Aeson.decode bytes)

-- | Persist the whole nested map as a nested JSON object.
saveUserData :: FilePath -> UserDataMap -> IO ()
saveUserData path m = LBS.writeFile path (Aeson.encode m)

-- | The persisted display preferences (milestone 17c):
-- @userId -> client -> prefs JSON value@, stored at
-- @stateDir/displayPreferences.json@.
type DisplayPrefsMap = HashMap Text (HashMap Text Value)

-- | Load @displayPreferences.json@; missing/unreadable -> empty map.
loadDisplayPrefs :: FilePath -> IO DisplayPrefsMap
loadDisplayPrefs path = do
  exists <- doesFileExist path
  if not exists
    then pure HM.empty
    else do
      result <- try (LBS.readFile path) :: IO (Either SomeException LBS.ByteString)
      pure $ case result of
        Left _      -> HM.empty
        Right bytes -> fromMaybe HM.empty (Aeson.decode bytes)

-- | Persist the display-preferences map as a nested JSON object.
saveDisplayPrefs :: FilePath -> DisplayPrefsMap -> IO ()
saveDisplayPrefs path m = LBS.writeFile path (Aeson.encode m)
