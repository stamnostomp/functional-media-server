module FMS.Modules.Devices.Monad
  ( MonadDevices (..)
  , devicesStateKey
  , devicesFilePath
  , loadDevicesIntoState
  , newDeviceId
  ) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Random (newStdGen, randomRs)

import FMS.Core.Monad (AppM, envConfig, getState, putState)
import FMS.Core.Types.Config (Config (..))
import FMS.Modules.Devices.Persistence (loadDevices, saveDevices)
import FMS.Modules.Devices.Types (Device (..), upsertDevice)

-- | Namespaced key under which the devices module stashes its
-- @TVar (HashMap Text Device)@ in the typed registry (docs/modules.org:
-- the core never knows module state).
devicesStateKey :: Text
devicesStateKey = "devices.map"

-- | Where the device map is persisted: @stateDir/devices.json@.
devicesFilePath :: Config -> FilePath
devicesFilePath cfg = configStateDir cfg </> "devices.json"

-- | Capability for the client-device registry (milestone 10c,
-- docs/todo.org section 8). The 'Bool' result of 'deleteDevice' means
-- "the device existed" — the routes map 'False' to 404.
class Monad m => MonadDevices m where
  listDevices   :: m [Device]                    -- ^ all devices, oldest id first (stable)
  getDevice     :: Text -> m (Maybe Device)      -- ^ by id
  upsertDeviceM :: Device -> m Device            -- ^ mint an id when empty, stamp lastSeenAt, persist
  deleteDevice  :: Text -> m Bool                -- ^ id -> existed?

instance MonadDevices AppM where
  listDevices = do
    tv <- devicesTVar
    sortOn devId . HM.elems <$> liftIO (readTVarIO tv)

  getDevice did = do
    tv <- devicesTVar
    liftIO (HM.lookup did <$> readTVarIO tv)

  upsertDeviceM d = do
    now <- liftIO getCurrentTime
    did <- if T.null (devId d) then liftIO newDeviceId else pure (devId d)
    let d' = d { devId = did, devLastSeenAt = Just now }
    _ <- modifyDevices (upsertDevice d')
    pure d'

  deleteDevice did = do
    m <- getDevice did
    case m of
      Nothing -> pure False
      Just _  -> do
        _ <- modifyDevices (HM.delete did)
        pure True

-- | A fresh device id: 16 random hex chars (same minting style as the
-- collections/playlists/auth ids).
newDeviceId :: IO Text
newDeviceId = do
  g <- newStdGen
  let rs = take 16 (randomRs (0, 15) g) :: [Int]
  pure (T.pack (map (\r -> "0123456789abcdef" !! r) rs))

-- | Module init: load the persisted @devices.json@ into a fresh TVar
-- under 'devicesStateKey' (creating the state dir if needed).
loadDevicesIntoState :: AppM ()
loadDevicesIntoState = do
  cfg <- asks envConfig
  liftIO (createDirectoryIfMissing True (configStateDir cfg))
  loaded <- liftIO (loadDevices (devicesFilePath cfg))
  tv <- liftIO (newTVarIO loaded)
  putState devicesStateKey tv

-- Internals ---------------------------------------------------------------

devicesTVar :: AppM (TVar (HashMap Text Device))
devicesTVar = do
  m <- getState devicesStateKey
  case m of
    Just tv -> pure tv
    Nothing -> do
      tv <- liftIO (newTVarIO (HM.empty :: HashMap Text Device))
      putState devicesStateKey tv
      pure tv

-- | Apply an update to the devices TVar AND persist the result to
-- @stateDir/devices.json@ after every mutation (the Stats/Metadata
-- module pattern: in-memory for reads, JSON on disk for restarts).
modifyDevices
  :: (HashMap Text Device -> HashMap Text Device)
  -> AppM (HashMap Text Device)
modifyDevices f = do
  cfg <- asks envConfig
  tv  <- devicesTVar
  updated <- liftIO $ atomically $ do
    modifyTVar' tv f
    readTVar tv
  liftIO (saveDevices (devicesFilePath cfg) updated)
  pure updated
