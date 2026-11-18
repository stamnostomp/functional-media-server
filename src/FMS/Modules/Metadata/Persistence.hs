module FMS.Modules.Metadata.Persistence
  ( loadMetadata
  , saveMetadata
  , loadMetadataSettings
  , saveMetadataSettings
  ) where

import Control.Exception (SomeException, try)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import System.Directory (doesFileExist)

import FMS.Modules.Metadata.Types (MediaMetadata, MetadataSettings (..))

-- | Load the persisted metadata store (@stateDir/metadata.json@): a bare
-- JSON object mapping item id -> 'MediaMetadata'. A missing or unreadable
-- file yields an empty map — metadata is best-effort, never fatal.
loadMetadata :: FilePath -> IO (HashMap Text MediaMetadata)
loadMetadata path = do
  exists <- doesFileExist path
  if not exists
    then pure HM.empty
    else do
      result <- try (LBS.readFile path) :: IO (Either SomeException LBS.ByteString)
      pure $ case result of
        Left _      -> HM.empty
        Right bytes -> fromMaybe HM.empty (Aeson.decode bytes)

-- | Persist the whole metadata map as a bare JSON object.
saveMetadata :: FilePath -> HashMap Text MediaMetadata -> IO ()
saveMetadata path meta = LBS.writeFile path (Aeson.encode meta)

-- | m35: load the persisted TMDB metadata settings
-- (@stateDir/metadata-settings.json@). A missing or unreadable file yields
-- 'Nothing' so the caller can seed from the env config.
loadMetadataSettings :: FilePath -> IO (Maybe MetadataSettings)
loadMetadataSettings path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      result <- try (LBS.readFile path) :: IO (Either SomeException LBS.ByteString)
      pure $ case result of
        Left _      -> Nothing
        Right bytes -> Aeson.decode bytes

-- | Persist the TMDB metadata settings (lowercase keys, strict write).
saveMetadataSettings :: FilePath -> MetadataSettings -> IO ()
saveMetadataSettings path s = LBS.writeFile path (Aeson.encode s)
