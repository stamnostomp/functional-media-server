module FMS.Modules.Indexers.Persistence
  ( loadIndexers
  , saveIndexers
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

import FMS.Modules.Indexers.Types (Indexer)

-- | Load the persisted indexer store (@stateDir/indexers.json@): a bare
-- JSON object mapping indexer id -> 'Indexer'. Missing/unreadable file
-- yields an empty map -- best-effort, never fatal. Strict read.
loadIndexers :: FilePath -> IO (HashMap Text Indexer)
loadIndexers path = do
  exists <- doesFileExist path
  if not exists
    then pure HM.empty
    else do
      result <- try (BS.readFile path) :: IO (Either SomeException BS.ByteString)
      pure $ case result of
        Left _      -> HM.empty
        Right bytes -> fromMaybe HM.empty (Aeson.decode (LBS.fromStrict bytes))

-- | Persist the whole indexer map as a bare JSON object.
saveIndexers :: FilePath -> HashMap Text Indexer -> IO ()
saveIndexers path indexers = LBS.writeFile path (Aeson.encode indexers)
