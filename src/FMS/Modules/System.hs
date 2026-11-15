module FMS.Modules.System
  ( systemModule
  ) where

import Control.Monad.Reader (asks)
import Data.Aeson (object, (.=))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types (status200)
import Network.Wai (Request, Response, responseLBS)
import Numeric (showHex)

import FMS (version)
import FMS.Core.Module (Module(..), get, jsonResponse, Segment(..))
import FMS.Core.Monad (AppM, getState, putState, envConfig)
import FMS.Core.Types.Config (Config(..))

-- | The system module: server info + health check (docs/todo.org section 1).
systemModule :: Module
systemModule = Module
  { moduleName = "system"
  , moduleInit = do
      cfg <- asks envConfig
      putState "system.id" (serverIdFor cfg)
  , moduleRoutes =
      [ get [Lit "System", Lit "Info", Lit "Public"] infoPublic
      , get [Lit "healthz"] healthz
      ]
  }

-- | @GET /System/Info/Public@ — public server info, Jellyfin-style.
infoPublic :: [(Text, Text)] -> Request -> AppM Response
infoPublic _captures _req = do
  serverId <- fromMaybe "unknown" <$> (getState "system.id" :: AppM (Maybe Text))
  pure (jsonResponse (object
    [ "ServerName" .= ("functional-media-server" :: Text)
    , "Version"    .= version
    , "Id"         .= serverId
    ]))

-- | @GET /healthz@ — liveness probe.
healthz :: [(Text, Text)] -> Request -> AppM Response
healthz _captures _req =
  pure (responseLBS status200 [("Content-Type", "text/plain; charset=utf-8")] "ok")

-- | A stable-ish server id derived from the config (library path + port),
-- rendered as a 32-char hex string like Jellyfin's device id.
serverIdFor :: Config -> Text
serverIdFor cfg =
  let h = hashText (T.pack (configLibraryPath cfg) <> ":" <> T.pack (show (configPort cfg)))
      hex = showHex h ""
  in T.pack (replicate (max 0 (32 - length hex)) '0' ++ hex)

hashText :: Text -> Integer
hashText = T.foldl' (\acc c -> acc * 31 + toInteger (fromEnum c)) 5381
