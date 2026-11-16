module FMS.Modules.Stats.Persistence
  ( loadStats
  , saveStats
  ) where

import Control.Exception (SomeException, try)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import System.Directory (doesFileExist)

import FMS.Core.Types.Progress (ItemStats)

-- | Load the persisted stats file (@stateDir/stats.json@): a bare JSON
-- object mapping item id -> 'ItemStats'. A missing or unreadable file
-- yields an empty map — stats are best-effort, never fatal.
loadStats :: FilePath -> IO (HashMap Text ItemStats)
loadStats path = do
  exists <- doesFileExist path
  if not exists
    then pure HM.empty
    else do
      result <- try (LBS.readFile path) :: IO (Either SomeException LBS.ByteString)
      pure $ case result of
        Left _      -> HM.empty
        Right bytes -> fromMaybe HM.empty (Aeson.decode bytes)

-- | Persist the whole stats map as a bare JSON object.
saveStats :: FilePath -> HashMap Text ItemStats -> IO ()
saveStats path stats = LBS.writeFile path (Aeson.encode stats)
