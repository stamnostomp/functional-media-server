module FMS.Modules.Trackers.Persistence
  ( loadTrackers
  , saveTrackers
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

import FMS.Modules.Trackers.Types (Tracker)

-- | Load the persisted tracker store (@stateDir/trackers.json@): a bare
-- JSON object mapping tracker id -> 'Tracker'. Missing/unreadable file
-- yields an empty map -- best-effort, never fatal. Strict read.
loadTrackers :: FilePath -> IO (HashMap Text Tracker)
loadTrackers path = do
  exists <- doesFileExist path
  if not exists
    then pure HM.empty
    else do
      result <- try (BS.readFile path) :: IO (Either SomeException BS.ByteString)
      pure $ case result of
        Left _      -> HM.empty
        Right bytes -> fromMaybe HM.empty (Aeson.decode (LBS.fromStrict bytes))

-- | Persist the whole tracker map as a bare JSON object.
saveTrackers :: FilePath -> HashMap Text Tracker -> IO ()
saveTrackers path trackers = LBS.writeFile path (Aeson.encode trackers)
