module FMS.Modules.Playlists.Persistence
  ( loadPlaylists
  , savePlaylists
  ) where

import Control.Exception (SomeException, try)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import System.Directory (doesFileExist)

import FMS.Modules.Playlists.Types (Playlist)

-- | Load the persisted playlist store (@stateDir/playlists.json@): a
-- bare JSON object mapping playlist id -> 'Playlist'. A missing or
-- unreadable file yields an empty map — playlists are best-effort,
-- never fatal.
loadPlaylists :: FilePath -> IO (HashMap Text Playlist)
loadPlaylists path = do
  exists <- doesFileExist path
  if not exists
    then pure HM.empty
    else do
      result <- try (LBS.readFile path) :: IO (Either SomeException LBS.ByteString)
      pure $ case result of
        Left _      -> HM.empty
        Right bytes -> fromMaybe HM.empty (Aeson.decode bytes)

-- | Persist the whole playlist map as a bare JSON object.
savePlaylists :: FilePath -> HashMap Text Playlist -> IO ()
savePlaylists path playlists = LBS.writeFile path (Aeson.encode playlists)
