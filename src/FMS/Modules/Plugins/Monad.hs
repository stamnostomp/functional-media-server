module FMS.Modules.Plugins.Monad
  ( MonadPlugins (..)
  , pluginsStateKey
  , hooksStateKey
  , pluginsFilePath
  , loadPluginsIntoState
  ) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

import FMS.Core.Monad (AppM, envConfig, getState, putState)
import FMS.Core.Types.Config (Config (..))
import FMS.Modules.Plugins.Persistence (loadPlugins, savePlugins)
import FMS.Modules.Plugins.Types (Plugin (..), builtinPlugins, overlayPlugins)

-- | Namespaced key under which the plugins module stashes its
-- @TVar (HashMap Text Plugin)@ in the typed registry (docs/modules.org:
-- the core never knows module state).
pluginsStateKey :: Text
pluginsStateKey = "plugins.map"

-- | Namespaced key for the in-process hook registry:
-- @TVar (HashMap Text [Text])@, hook name -> registered plugin ids.
hooksStateKey :: Text
hooksStateKey = "plugins.hooks"

-- | Where the plugin map is persisted: @stateDir/plugins.json@.
pluginsFilePath :: Config -> FilePath
pluginsFilePath cfg = configStateDir cfg </> "plugins.json"

-- | Capability for the plugin registry skeleton (plugin system,
-- docs/todo.org section on plugins). 'setPluginEnabled' returns 'False'
-- when the id is unknown -- the routes map that to 404.
class Monad m => MonadPlugins m where
  listPlugins       :: m [Plugin]                 -- ^ all plugins, sorted by Name
  getPlugin         :: Text -> m (Maybe Plugin)   -- ^ by id
  setPluginEnabled  :: Text -> Bool -> m Bool     -- ^ id -> enabled -> existed?

instance MonadPlugins AppM where
  listPlugins = do
    tv <- pluginsTVar
    sortOn plugName . HM.elems <$> liftIO (readTVarIO tv)

  getPlugin pid = do
    tv <- pluginsTVar
    liftIO (HM.lookup pid <$> readTVarIO tv)

  setPluginEnabled pid enabled = do
    m <- getPlugin pid
    case m of
      Nothing -> pure False
      Just p  -> do
        _ <- modifyPlugins (HM.insert pid (p { plugEnabled = enabled }))
        pure True

-- | Module init: load the persisted @plugins.json@, overlay it onto the
-- built-ins (persisted enable state + unknown ids survive; missing
-- built-ins get their defaults), seed the file on first boot, and stash
-- the map in a TVar under 'pluginsStateKey'. Also registers an empty
-- hook map under 'hooksStateKey'.
loadPluginsIntoState :: AppM ()
loadPluginsIntoState = do
  cfg <- asks envConfig
  liftIO (createDirectoryIfMissing True (configStateDir cfg))
  let path = pluginsFilePath cfg
  exists <- liftIO (doesFileExist path)
  loaded <- liftIO (loadPlugins path)
  let merged = overlayPlugins builtinPlugins loaded
  unless exists (liftIO (savePlugins path merged))  -- seed on first boot
  tv <- liftIO (newTVarIO merged)
  putState pluginsStateKey tv
  hooksTv <- liftIO (newTVarIO (HM.empty :: HashMap Text [Text]))
  putState hooksStateKey hooksTv

-- Internals ---------------------------------------------------------------

pluginsTVar :: AppM (TVar (HashMap Text Plugin))
pluginsTVar = do
  m <- getState pluginsStateKey
  case m of
    Just tv -> pure tv
    Nothing -> do
      tv <- liftIO (newTVarIO (HM.fromList [ (plugId b, b) | b <- builtinPlugins ]))
      putState pluginsStateKey tv
      pure tv

-- | Apply an update to the plugins TVar AND persist the result to
-- @stateDir/plugins.json@ after every mutation (the Stats/Metadata
-- module pattern: in-memory for reads, JSON on disk for restarts).
modifyPlugins
  :: (HashMap Text Plugin -> HashMap Text Plugin)
  -> AppM (HashMap Text Plugin)
modifyPlugins f = do
  cfg <- asks envConfig
  tv  <- pluginsTVar
  updated <- liftIO $ atomically $ do
    modifyTVar' tv f
    readTVar tv
  liftIO (savePlugins (pluginsFilePath cfg) updated)
  pure updated
