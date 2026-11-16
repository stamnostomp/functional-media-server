module FMS.Modules.Webhooks.Persistence
  ( loadWebhooks
  , saveWebhooks
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

import FMS.Modules.Webhooks.Types (Webhook)

-- | Load the persisted webhook store (@stateDir/webhooks.json@): a bare
-- JSON object mapping webhook id -> 'Webhook'. Missing/unreadable ->
-- empty map (best-effort, never fatal). Strict 'BS.readFile' like
-- 'FMS.Modules.Library.Folders.loadFolders'.
loadWebhooks :: FilePath -> IO (HashMap Text Webhook)
loadWebhooks path = do
  exists <- doesFileExist path
  if not exists
    then pure HM.empty
    else do
      result <- try (BS.readFile path) :: IO (Either SomeException BS.ByteString)
      pure $ case result of
        Left _      -> HM.empty
        Right bytes -> fromMaybe HM.empty (Aeson.decode (LBS.fromStrict bytes))

-- | Persist the whole webhook map as a bare JSON object.
saveWebhooks :: FilePath -> HashMap Text Webhook -> IO ()
saveWebhooks path webhooks = LBS.writeFile path (Aeson.encode webhooks)
