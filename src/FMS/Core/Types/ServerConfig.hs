module FMS.Core.Types.ServerConfig
  ( ServerConfig(..)
  , ServerConfigPatch(..)
  , defaultServerConfig
  , serverConfigFilePath
  , loadServerConfig
  , saveServerConfig
  , serverConfigToPatch
  , applyServerConfigPatch
  , mergeServerConfig
  ) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:?), (.!=), (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

-- | The persistent server configuration (docs/todo.org section 1,
-- milestone 1b): @stateDir/config.json@ on disk, overridable at runtime
-- via @POST /System/Configuration@ and by env vars at startup (env wins).
data ServerConfig = ServerConfig
  { scServerName  :: !Text      -- ^ display name (FMS_SERVER_NAME)
  , scRequireAuth :: !Bool      -- ^ auth gate for non-open paths (FMS_REQUIRE_AUTH)
  , scLibraryPath :: !FilePath  -- ^ media library root (MEDIA_LIBRARY_PATH)
  , scPort        :: !Int       -- ^ HTTP port (MEDIA_SERVER_PORT)
  , scStartupCompleted :: !Bool -- ^ milestone 10b: first-run wizard done
  , scLoginDisclaimer  :: !Text -- ^ milestone 10b: branding login disclaimer
  , scCustomCss        :: !Text -- ^ milestone 10b: branding custom CSS
  , scUiDir            :: !(Maybe FilePath) -- ^ serve the Elm UI from this dir (config.json key "uiDir"; env FMS_UI_DIR / --ui-dir)
  } deriving (Show, Eq)

instance ToJSON ServerConfig where
  toJSON sc = object
    [ "ServerName"  .= scServerName sc
    , "RequireAuth" .= scRequireAuth sc
    , "LibraryPath" .= scLibraryPath sc
    , "Port"        .= scPort sc
    , "StartupCompleted" .= scStartupCompleted sc
    , "LoginDisclaimer"  .= scLoginDisclaimer sc
    , "CustomCss"        .= scCustomCss sc
    , "uiDir"            .= scUiDir sc
    ]

-- | Every field optional with the defaults as fallback, so a pre-10b
-- (or otherwise partial) config.json decodes directly as a 'ServerConfig'.
instance FromJSON ServerConfig where
  parseJSON = withObject "ServerConfig" $ \o ->
    ServerConfig
      <$> o .:? "ServerName"  .!= scServerName defaultServerConfig
      <*> o .:? "RequireAuth" .!= scRequireAuth defaultServerConfig
      <*> o .:? "LibraryPath" .!= scLibraryPath defaultServerConfig
      <*> o .:? "Port"        .!= scPort defaultServerConfig
      <*> o .:? "StartupCompleted" .!= scStartupCompleted defaultServerConfig
      <*> o .:? "LoginDisclaimer"  .!= scLoginDisclaimer defaultServerConfig
      <*> o .:? "CustomCss"        .!= scCustomCss defaultServerConfig
      <*> (o .:? "uiDir" <|> o .:? "UiDir")

-- | A partial update: every field optional. @POST /System/Configuration@
-- decodes the body into this and merges it over the current config, so a
-- client can change one field without resending the others.
data ServerConfigPatch = ServerConfigPatch
  { scpServerName  :: Maybe Text
  , scpRequireAuth :: Maybe Bool
  , scpLibraryPath :: Maybe FilePath
  , scpPort        :: Maybe Int
  , scpStartupCompleted :: Maybe Bool  -- ^ milestone 10b
  , scpLoginDisclaimer  :: Maybe Text  -- ^ milestone 10b
  , scpCustomCss        :: Maybe Text  -- ^ milestone 10b
  , scpUiDir            :: Maybe FilePath
  } deriving (Show, Eq)

-- | Patch convention: every field optional; a missing OR null field is
-- 'Nothing' and means "keep the current value" (see 'applyServerConfigPatch').
instance FromJSON ServerConfigPatch where
  parseJSON = withObject "ServerConfigPatch" $ \o ->
    ServerConfigPatch
      <$> o .:? "ServerName"
      <*> o .:? "RequireAuth"
      <*> o .:? "LibraryPath"
      <*> o .:? "Port"
      <*> o .:? "StartupCompleted"
      <*> o .:? "LoginDisclaimer"
      <*> o .:? "CustomCss"
      <*> (o .:? "uiDir" <|> o .:? "UiDir")

instance ToJSON ServerConfigPatch where
  toJSON p = object
    [ "ServerName"  .= scpServerName p
    , "RequireAuth" .= scpRequireAuth p
    , "LibraryPath" .= scpLibraryPath p
    , "Port"        .= scpPort p
    , "StartupCompleted" .= scpStartupCompleted p
    , "LoginDisclaimer"  .= scpLoginDisclaimer p
    , "CustomCss"        .= scpCustomCss p
    , "uiDir"            .= scpUiDir p
    ]

-- | The built-in defaults (same values the env-var defaults had before
-- config.json existed; milestone 10b adds the branding/wizard defaults).
defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig
  { scServerName  = "functional-media-server"
  , scRequireAuth = False
  , scLibraryPath = "./media"
  , scPort        = 8080
  , scStartupCompleted = False
  , scLoginDisclaimer  = ""
  , scCustomCss        = ""
  , scUiDir            = Nothing
  }

-- | Where the config file lives: @stateDir \/ \"config.json\"@.
serverConfigFilePath :: FilePath -> FilePath
serverConfigFilePath stateDir = stateDir </> "config.json"

-- | Load the config file. A missing, unreadable, or malformed file yields
-- the defaults; a present file is decoded as a (possibly partial) patch
-- and applied over the defaults, so old/partial config files keep working.
loadServerConfig :: FilePath -> IO ServerConfig
loadServerConfig path = do
  exists <- doesFileExist path
  if not exists
    then pure defaultServerConfig
    else do
      r <- try (LBS.readFile path) :: IO (Either SomeException LBS.ByteString)
      pure $ case r of
        Left _  -> defaultServerConfig
        Right bs ->
          case Aeson.eitherDecode bs of
            Right patch -> applyServerConfigPatch defaultServerConfig patch
            Left _      -> defaultServerConfig

-- | Persist the whole config as JSON.
saveServerConfig :: FilePath -> ServerConfig -> IO ()
saveServerConfig path sc = LBS.writeFile path (Aeson.encode sc)

-- | Every field of a config as a full patch.
serverConfigToPatch :: ServerConfig -> ServerConfigPatch
serverConfigToPatch sc = ServerConfigPatch
  { scpServerName  = Just (scServerName sc)
  , scpRequireAuth = Just (scRequireAuth sc)
  , scpLibraryPath = Just (scLibraryPath sc)
  , scpPort        = Just (scPort sc)
  , scpStartupCompleted = Just (scStartupCompleted sc)
  , scpLoginDisclaimer  = Just (scLoginDisclaimer sc)
  , scpCustomCss        = Just (scCustomCss sc)
  , scpUiDir            = scUiDir sc
  }

-- | Pure merge: the patch's set fields override the base's, unset fields
-- are kept from the base.
applyServerConfigPatch :: ServerConfig -> ServerConfigPatch -> ServerConfig
applyServerConfigPatch base p = ServerConfig
  { scServerName  = fromMaybe (scServerName base)  (scpServerName p)
  , scRequireAuth = fromMaybe (scRequireAuth base) (scpRequireAuth p)
  , scLibraryPath = fromMaybe (scLibraryPath base) (scpLibraryPath p)
  , scPort        = fromMaybe (scPort base)       (scpPort p)
  , scStartupCompleted = fromMaybe (scStartupCompleted base) (scpStartupCompleted p)
  , scLoginDisclaimer  = fromMaybe (scLoginDisclaimer base)  (scpLoginDisclaimer p)
  , scCustomCss        = fromMaybe (scCustomCss base)        (scpCustomCss p)
  , scUiDir            = maybe (scUiDir base) Just (scpUiDir p)  -- patch Nothing = keep
  }

-- | Pure merge of two full configs: the second one's fields win.
mergeServerConfig :: ServerConfig -> ServerConfig -> ServerConfig
mergeServerConfig base overrides =
  applyServerConfigPatch base (serverConfigToPatch overrides)
