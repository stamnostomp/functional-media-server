module FMS.Modules.Plugins.Persistence
  ( loadPlugins
  , savePlugins
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

import FMS.Modules.Plugins.Types (Plugin)

-- | Load the persisted plugin store (@stateDir/plugins.json@): a bare
-- JSON object mapping plugin id -> 'Plugin'. A missing or unreadable file
-- yields an empty map -- best-effort, never fatal. Strict read
-- ('BS.readFile', like 'FMS.Modules.Library.Folders.loadFolders').
loadPlugins :: FilePath -> IO (HashMap Text Plugin)
loadPlugins path = do
  exists <- doesFileExist path
  if not exists
    then pure HM.empty
    else do
      result <- try (BS.readFile path) :: IO (Either SomeException BS.ByteString)
      pure $ case result of
        Left _      -> HM.empty
        Right bytes -> fromMaybe HM.empty (Aeson.decode (LBS.fromStrict bytes))

-- | Persist the whole plugin map as a bare JSON object.
savePlugins :: FilePath -> HashMap Text Plugin -> IO ()
savePlugins path plugins = LBS.writeFile path (Aeson.encode plugins)
