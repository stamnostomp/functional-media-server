module FMS.Modules.Collections.Persistence
  ( loadCollections
  , saveCollections
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

import FMS.Modules.Collections.Types (Collection)

-- | Load the persisted collection store (@stateDir/collections.json@): a
-- bare JSON object mapping collection id -> 'Collection'. A missing or
-- unreadable file yields an empty map — collections are best-effort,
-- never fatal. Reads the file strictly ('BS.readFile', like
-- 'FMS.Modules.Library.Folders.loadFolders').
loadCollections :: FilePath -> IO (HashMap Text Collection)
loadCollections path = do
  exists <- doesFileExist path
  if not exists
    then pure HM.empty
    else do
      result <- try (BS.readFile path) :: IO (Either SomeException BS.ByteString)
      pure $ case result of
        Left _      -> HM.empty
        Right bytes -> fromMaybe HM.empty (Aeson.decode (LBS.fromStrict bytes))

-- | Persist the whole collection map as a bare JSON object.
saveCollections :: FilePath -> HashMap Text Collection -> IO ()
saveCollections path collections = LBS.writeFile path (Aeson.encode collections)
