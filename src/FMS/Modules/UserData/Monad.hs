module FMS.Modules.UserData.Monad
  ( MonadUserData (..)
  , userDataStateKey
  , userDataFilePath
  , loadUserDataIntoState
  , modifyUserDataMap
  , markPlayed
  , setFavorite
  , setPosition
  ) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))

import FMS.Core.Monad (AppEnv (..), AppM, getState, putState)
import FMS.Core.Types.Config (Config (..))
import FMS.Modules.UserData.Persistence (UserDataMap, loadUserData, saveUserData)
import FMS.Modules.UserData.Types (UserData (..), emptyUserData)

-- | Namespaced key under which the user-data module stashes its
-- @TVar UserDataMap@ in the typed registry (docs/modules.org: the core
-- never knows module state).
userDataStateKey :: Text
userDataStateKey = "userdata.items"

-- | Where the nested map is persisted: @stateDir/userdata.json@.
userDataFilePath :: Config -> FilePath
userDataFilePath cfg = configStateDir cfg </> "userdata.json"

-- | Capability for per-user item data (docs/todo.org section 9,
-- milestone 5c). Handlers require @MonadUserData m => ...@.
class Monad m => MonadUserData m where
  getUserData    :: Text -> Text -> m UserData                  -- ^ userId, itemId (empty default)
  updateUserData :: Text -> Text -> (UserData -> UserData) -> m UserData  -- ^ userId, itemId, update
  listUserData   :: Text -> m (HashMap Text UserData)           -- ^ all item data of one user

instance MonadUserData AppM where
  getUserData uid iid = do
    m <- currentUserDataMap
    pure (fromMaybe emptyUserData (HM.lookup iid =<< HM.lookup uid m))

  updateUserData uid iid f = do
    updated <- modifyUserDataMap $ \m ->
      let inner = fromMaybe HM.empty (HM.lookup uid m)
          old   = fromMaybe emptyUserData (HM.lookup iid inner)
      in HM.insert uid (HM.insert iid (f old) inner) m
    pure (fromMaybe emptyUserData (HM.lookup iid =<< HM.lookup uid updated))

  listUserData uid = fromMaybe HM.empty . HM.lookup uid <$> currentUserDataMap

-- Pure helpers (exported for tests) ----------------------------------------

-- | Mark an item played/unplayed. Marking played also bumps the play count
-- (Jellyfin semantics); unplaying only clears the flag.
markPlayed :: Bool -> UserData -> UserData
markPlayed True  ud = ud { udPlayed = True, udPlayCount = udPlayCount ud + 1 }
markPlayed False ud = ud { udPlayed = False }

-- | Set/clear the favorite (liked) flag.
setFavorite :: Bool -> UserData -> UserData
setFavorite b ud = ud { udFavorite = b }

-- | Set the resume position (seconds into the item).
setPosition :: Double -> UserData -> UserData
setPosition p ud = ud { udPlaybackPosition = Just p }

-- Internals ---------------------------------------------------------------

-- | Apply an update to the nested TVar AND persist the result to
-- @stateDir/userdata.json@ after every update (the same pattern as the
-- stats module: in-memory state for reads, JSON on disk for restarts).
-- Returns the full updated map.
modifyUserDataMap :: (UserDataMap -> UserDataMap) -> AppM UserDataMap
modifyUserDataMap f = do
  cfg <- asks envConfig
  tv <- ensureUserDataTVar
  updated <- liftIO $ atomically $ do
    modifyTVar' tv f
    readTVar tv
  liftIO (saveUserData (userDataFilePath cfg) updated)
  pure updated

ensureUserDataTVar :: AppM (TVar UserDataMap)
ensureUserDataTVar = do
  mTv <- getState userDataStateKey
  case mTv of
    Just tv -> pure tv
    Nothing -> do
      tv <- liftIO (newTVarIO HM.empty)
      putState userDataStateKey tv
      pure tv

currentUserDataMap :: AppM UserDataMap
currentUserDataMap = do
  mTv <- getState userDataStateKey
  case mTv of
    Just tv -> liftIO (readTVarIO tv)
    Nothing -> pure HM.empty

-- | Reads the persisted user-data file into a fresh TVar under
-- 'userDataStateKey' (creating the state dir if needed). Called once from
-- the user-data module's @moduleInit@.
loadUserDataIntoState :: AppM ()
loadUserDataIntoState = do
  cfg <- asks envConfig
  liftIO (createDirectoryIfMissing True (configStateDir cfg))
  loaded <- liftIO (loadUserData (userDataFilePath cfg))
  tv <- liftIO (newTVarIO loaded)
  putState userDataStateKey tv
