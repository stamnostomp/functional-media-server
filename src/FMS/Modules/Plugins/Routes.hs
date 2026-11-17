module FMS.Modules.Plugins.Routes
  ( pluginsRoutes
  ) where

import Control.Monad.Except (throwError)
import Data.Text (Text)
import Network.Wai (Request, Response)

import FMS.Core.Module (Route, Segment (..), get, jsonResponse, post)
import FMS.Core.Monad (AppError (..), AppM)
import FMS.Modules.Auth.Monad (requireAdmin)
import FMS.Modules.Plugins.Monad (MonadPlugins (..))

-- | The plugin registry API (plugin system skeleton, Jellyfin =/Plugins=
-- parity). GET routes go through the normal auth gate (any authenticated
-- user); the Enable/Disable mutations are admin-only (milestone 16
-- pattern). Unknown plugin ids are 404.
pluginsRoutes :: [Route]
pluginsRoutes =
  [ get  [Lit "Plugins"] listPluginsHandler
  , get  [Lit "Plugins", Capture "id"] getPluginHandler
  , post [Lit "Plugins", Capture "id", Lit "Enable"] (setEnabledHandler True)
  , post [Lit "Plugins", Capture "id", Lit "Disable"] (setEnabledHandler False)
  ]

-- Handlers ------------------------------------------------------------------

-- | @GET \/Plugins@ — 200 with all plugins sorted by name.
listPluginsHandler :: [(Text, Text)] -> Request -> AppM Response
listPluginsHandler _captures _req = jsonResponse <$> listPlugins

-- | @GET \/Plugins\/:id@ — 200 with the plugin, 404 when unknown.
getPluginHandler :: [(Text, Text)] -> Request -> AppM Response
getPluginHandler captures _req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just pid -> do
      m <- getPlugin pid
      case m of
        Nothing -> throwError (NotFound ("no plugin with id " <> pid))
        Just p  -> pure (jsonResponse p)

-- | Shared body of @POST /Plugins/:id/Enable@ and @.../Disable@ (admin):
-- persist the new enabled state, 200 with the updated plugin, 404 when
-- the id is unknown.
setEnabledHandler :: Bool -> [(Text, Text)] -> Request -> AppM Response
setEnabledHandler enabled captures req = do
  _ <- requireAdmin req
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just pid -> do
      ok <- setPluginEnabled pid enabled
      if not ok
        then throwError (NotFound ("no plugin with id " <> pid))
        else do
          m <- getPlugin pid
          case m of
            Just p  -> pure (jsonResponse p)
            Nothing -> throwError (NotFound ("no plugin with id " <> pid))
