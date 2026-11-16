module FMS.Modules.Library.Folders
  ( loadFolders
  , saveFolders
  , foldersFilePath
  , defaultFolders
  , nextFolderId
  ) where

import Control.Exception (SomeException, try)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import Text.Read (readMaybe)

import FMS.Core.Types.Config (Config (..))
import FMS.Core.Types.MediaFolder (MediaFolder (..))

-- | Where the folder map is persisted: @stateDir/folders.json@.
foldersFilePath :: Config -> FilePath
foldersFilePath cfg = configStateDir cfg </> "folders.json"

-- | Load the persisted folder store: a bare JSON object mapping folder id
-- -> 'MediaFolder'. A missing or unreadable file yields an empty map.
loadFolders :: FilePath -> IO (HashMap Text MediaFolder)
loadFolders path = do
  exists <- doesFileExist path
  if not exists
    then pure HM.empty
    else do
      -- Strict read: the handle is closed before we return, so a
      -- saveFolders write-open on the same file right after a restart
      -- cannot hit "resource busy (file is locked)".
      result <- try (BS.readFile path) :: IO (Either SomeException BS.ByteString)
      pure $ case result of
        Left _      -> HM.empty
        Right bytes -> fromMaybe HM.empty (Aeson.decode (LBS.fromStrict bytes))

-- | Persist the whole folder map as a bare JSON object.
saveFolders :: FilePath -> HashMap Text MediaFolder -> IO ()
saveFolders path folders = LBS.writeFile path (Aeson.encode folders)

-- | The folder set used when no @folders.json@ exists yet: a single
-- "mixed" folder rooted at the configured library path, with id "1"
-- (pre-2c state dirs migrate to this via the 'MediaItem' SafeCopy
-- migration, which tags old items with folder "1").
defaultFolders :: Config -> HashMap Text MediaFolder
defaultFolders cfg = HM.fromList [ (folderId f, f) ]
  where
    f = MediaFolder
      { folderId   = "1"
      , folderName = "Media"
      , folderPath = configLibraryPath cfg
      , folderType = "mixed"
      }

-- | Mint the next folder id: one past the highest purely-numeric id
-- (numeric ids keep folder "1" first and stay stable across add/remove,
-- unlike a count-based scheme).
nextFolderId :: HashMap Text MediaFolder -> Text
nextFolderId folders =
  let numericIds = [ i | (k, _) <- HM.toList folders
                       , Just i <- [readMaybe (T.unpack k) :: Maybe Int] ]
  in T.pack (show (1 + max 0 (foldr max 0 numericIds)))
