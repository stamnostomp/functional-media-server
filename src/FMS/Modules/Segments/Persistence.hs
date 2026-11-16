module FMS.Modules.Segments.Persistence
  ( loadSegments
  , saveSegments
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

import FMS.Modules.Segments.Types (MediaSegment)

-- | Load the persisted segment store (@stateDir/mediaSegments.json@): a
-- bare JSON object mapping item id -> [MediaSegment]. A missing or
-- unreadable file yields an empty map — segments are best-effort, never
-- fatal. Reads the file strictly ('BS.readFile', like
-- 'FMS.Modules.Library.Folders.loadFolders').
loadSegments :: FilePath -> IO (HashMap Text [MediaSegment])
loadSegments path = do
  exists <- doesFileExist path
  if not exists
    then pure HM.empty
    else do
      result <- try (BS.readFile path) :: IO (Either SomeException BS.ByteString)
      pure $ case result of
        Left _      -> HM.empty
        Right bytes -> fromMaybe HM.empty (Aeson.decode (LBS.fromStrict bytes))

-- | Persist the whole segment map as a bare JSON object.
saveSegments :: FilePath -> HashMap Text [MediaSegment] -> IO ()
saveSegments path segments = LBS.writeFile path (Aeson.encode segments)
