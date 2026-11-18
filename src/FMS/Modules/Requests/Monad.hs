module FMS.Modules.Requests.Monad
  ( MonadRequests (..)
  , requestsStateKey
  , requestsFilePath
  , loadRequestsIntoState
  , newRequestId
  , requestSettingsStateKey
  , requestSettingsFilePath
  , loadRequestSettingsIntoState
  , getRequestSettings
  , updateRequestSettings
  ) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Random (newStdGen, randomRs)

import FMS.Core.Monad (AppM, envConfig, getState, putState)
import FMS.Core.Types.Config (Config (..))
import FMS.Modules.Requests.Persistence (loadRequestSettings, loadRequests, saveRequestSettings, saveRequests)
import FMS.Modules.Requests.Types (Request (..), RequestSettings (..), RequestStatus (..), settingsFromConfig)

-- | Namespaced key under which the requests module stashes its
-- @TVar (HashMap Text Request)@ in the typed registry.
requestsStateKey :: Text
requestsStateKey = "requests.map"

-- | Where the request map is persisted: @stateDir/requests.json@.
requestsFilePath :: Config -> FilePath
requestsFilePath cfg = configStateDir cfg </> "requests.json"

-- | Namespaced key for the download-client settings TVar (m27).
requestSettingsStateKey :: Text
requestSettingsStateKey = "requests.settings"

-- | Where the settings are persisted: @stateDir/requests-settings.json@.
requestSettingsFilePath :: Config -> FilePath
requestSettingsFilePath cfg = configStateDir cfg </> "requests-settings.json"

-- | Capability for the media-request queue (m20a CRUD core).
class Monad m => MonadRequests m where
  listRequests       :: m [Request]                          -- ^ all, newest first
  getRequest         :: Text -> m (Maybe Request)            -- ^ by id
  insertRequest      :: Request -> m Request                  -- ^ persisted insert
  setRequestStatus   :: Text -> RequestStatus -> Maybe Text -> m (Maybe Request)
    -- ^ id -> new status -> approving admin (bump 'reqUpdatedAt'); Nothing when unknown
  setRequestFailed   :: Text -> Text -> m (Maybe Request)
    -- ^ id -> error text: set status Failed + 'reqError', bump 'reqUpdatedAt', persisted
  deleteRequest      :: Text -> m Bool                       -- ^ id -> existed?

instance MonadRequests AppM where
  listRequests = do
    tv <- requestsTVar
    sortOn (Down . reqCreatedAt) . HM.elems <$> liftIO (readTVarIO tv)

  getRequest rid = do
    tv <- requestsTVar
    liftIO (HM.lookup rid <$> readTVarIO tv)

  insertRequest r = do
    _ <- modifyRequests (HM.insert (reqId r) r)
    pure r

  setRequestStatus rid status mApprover = do
    now <- liftIO getCurrentTime
    m <- getRequest rid
    case m of
      Nothing -> pure Nothing
      Just r  -> do
        let r' = r { reqStatus = status
                   , reqUpdatedAt = now
                   , reqApprovedBy = case status of
                       Approved -> mApprover
                       _        -> reqApprovedBy r
                   }
        _ <- modifyRequests (HM.insert rid r')
        pure (Just r')

  setRequestFailed rid err = do
    now <- liftIO getCurrentTime
    m <- getRequest rid
    case m of
      Nothing -> pure Nothing
      Just r  -> do
        let r' = r { reqStatus = Failed, reqError = Just err, reqUpdatedAt = now }
        _ <- modifyRequests (HM.insert rid r')
        pure (Just r')

  deleteRequest rid = do
    m <- getRequest rid
    case m of
      Nothing -> pure False
      Just _  -> do
        _ <- modifyRequests (HM.delete rid)
        pure True

-- | Module init: load the persisted @requests.json@ into a fresh TVar
-- under 'requestsStateKey' (creating the state dir if needed).
loadRequestsIntoState :: AppM ()
loadRequestsIntoState = do
  cfg <- asks envConfig
  liftIO (createDirectoryIfMissing True (configStateDir cfg))
  loaded <- liftIO (loadRequests (requestsFilePath cfg))
  tv <- liftIO (newTVarIO loaded)
  putState requestsStateKey tv

-- | m27: module init for the download-client settings. When
-- @requests-settings.json@ is absent (first boot) the settings are seeded
-- from the env-derived config and saved; when present, the saved values
-- win (effective settings).
loadRequestSettingsIntoState :: AppM ()
loadRequestSettingsIntoState = do
  cfg <- asks envConfig
  liftIO (createDirectoryIfMissing True (configStateDir cfg))
  let path = requestSettingsFilePath cfg
  mSaved <- liftIO (loadRequestSettings path)
  s <- case mSaved of
    Just saved -> pure saved
    Nothing    -> do
      let seeded = settingsFromConfig cfg
      liftIO (saveRequestSettings path seeded)
      pure seeded
  tv <- liftIO (newTVarIO s)
  putState requestSettingsStateKey tv

-- | m27: the effective (saved, else env-seeded) download-client settings.
getRequestSettings :: AppM RequestSettings
getRequestSettings = do
  m <- getState requestSettingsStateKey
  case m of
    Just tv -> liftIO (readTVarIO tv)
    Nothing -> do
      -- lazy init (moduleInit normally ran): seed from env config
      cfg <- asks envConfig
      let s = settingsFromConfig cfg
      tv <- liftIO (newTVarIO s)
      putState requestSettingsStateKey tv
      pure s

-- | m27: apply an update to the settings TVar AND persist to
-- @stateDir/requests-settings.json@.
updateRequestSettings :: (RequestSettings -> RequestSettings) -> AppM RequestSettings
updateRequestSettings f = do
  cfg <- asks envConfig
  s0 <- getRequestSettings
  m <- getState requestSettingsStateKey
  case m of
    Nothing -> pure s0
    Just tv -> do
      updated <- liftIO $ atomically $ do
        modifyTVar' tv f
        readTVar tv
      liftIO (saveRequestSettings (requestSettingsFilePath cfg) updated)
      pure updated

-- Internals ---------------------------------------------------------------

requestsTVar :: AppM (TVar (HashMap Text Request))
requestsTVar = do
  m <- getState requestsStateKey
  case m of
    Just tv -> pure tv
    Nothing -> do
      tv <- liftIO (newTVarIO HM.empty)
      putState requestsStateKey tv
      pure tv

-- | Apply an update to the requests TVar AND persist the result to
-- @stateDir/requests.json@ after every mutation (the Plugins/Stats
-- pattern: in-memory for reads, JSON on disk for restarts).
modifyRequests
  :: (HashMap Text Request -> HashMap Text Request)
  -> AppM (HashMap Text Request)
modifyRequests f = do
  cfg <- asks envConfig
  tv  <- requestsTVar
  updated <- liftIO $ atomically $ do
    modifyTVar' tv f
    readTVar tv
  liftIO (saveRequests (requestsFilePath cfg) updated)
  pure updated

-- | A fresh request id: 16 random hex chars (same minting style as the
-- webhook/collection ids).
newRequestId :: IO Text
newRequestId = do
  g <- newStdGen
  let rs = take 16 (randomRs (0, 15) g) :: [Int]
  pure (T.pack (map (\r -> "0123456789abcdef" !! r) rs))
