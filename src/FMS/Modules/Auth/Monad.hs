module FMS.Modules.Auth.Monad
  ( MonadAuth (..)
  , authUsersKey
  , authTokensKey
  , initAuthState
  , modifyAuthUsers
  , hashPassword
  , verifyPassword
  , mintToken
  ) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (find, sortOn)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing)
import Text.Read (readMaybe)

import FMS.Core.Monad (AppError (..), AppM, MonadLog (..), envConfig, getState, putState)
import FMS.Core.Types.Config (Config (..))
import FMS.Core.Types.User (User (..))
import FMS.Modules.Auth.State
  ( hashPassword
  , loadOrBootstrapUsers
  , mintToken
  , saveUsers
  , usersFilePath
  , verifyPassword
  )

-- | Namespaced keys under which the auth module stashes its TVars in the
-- typed registry (docs/modules.org: the core never knows module state).
-- The router's auth gate reads these same two keys.
authUsersKey :: Text
authUsersKey = "auth.users"

authTokensKey :: Text
authTokensKey = "auth.tokens"

-- | The auth capability (docs/todo.org section 8). Handlers in this module
-- (and any future module) require @MonadAuth m => ...@.
class Monad m => MonadAuth m where
  listUsers    :: m [User]
  getUser      :: Text -> m (Maybe User)
  createUser   :: Text -> Text -> Bool -> m User   -- ^ name, password, isAdmin
  deleteUser   :: Text -> m ()
  setPassword  :: Text -> Text -> m ()             -- ^ userId, new password
  addApiKey    :: Text -> m Text                   -- ^ userId -> fresh API key
  authenticate :: Text -> Text -> m (Maybe (User, Text))  -- ^ name, password -> (user, session token)
  resolveToken :: Text -> m (Maybe User)           -- ^ session token OR api key -> user
  listSessions :: m [(Text, Text)]                 -- ^ (token, userName) for live session tokens

instance MonadAuth AppM where
  listUsers = do
    users <- currentUsers
    pure (sortOn userId (HM.elems users))

  getUser i = HM.lookup i <$> currentUsers

  createUser name pw admin = do
    users <- currentUsers
    let uid = nextUserId users
    h <- liftIO (hashPassword pw)
    let u = User uid name h admin []
    _ <- modifyAuthUsers (HM.insert uid u)
    logInfo ("auth: created user " <> name <> " (id " <> uid <> ")")
    pure u

  deleteUser i = do
    _ <- modifyAuthUsers (HM.delete i)
    logInfo ("auth: deleted user " <> i)
    pure ()

  setPassword i pw = do
    h <- liftIO (hashPassword pw)
    _ <- modifyAuthUsers (HM.adjust (\u -> u { passwordHash = h }) i)
    pure ()

  addApiKey i = do
    tok <- liftIO mintToken
    _ <- modifyAuthUsers (HM.adjust (\u -> u { apiKeys = apiKeys u ++ [tok] }) i)
    pure tok

  authenticate name pw = do
    users <- currentUsers
    -- usernames are matched case-insensitively, like Jellyfin
    case find (\u -> T.toLower (userName u) == T.toLower name) (HM.elems users) of
      Just u
        | verifyPassword (passwordHash u) pw -> do
            tok <- liftIO mintToken
            tt  <- tokensTVar
            liftIO (atomically (modifyTVar' tt (HM.insert tok (userId u))))
            pure (Just (u, tok))
        | otherwise -> pure Nothing
      Nothing -> pure Nothing

  resolveToken tok = do
    toks <- currentTokens
    usrs <- currentUsers
    pure $ case HM.lookup tok toks of
      Just uid -> HM.lookup uid usrs
      Nothing  -> find (\u -> tok `elem` apiKeys u) (HM.elems usrs)

  listSessions = do
    toks <- currentTokens
    usrs <- currentUsers
    pure [ (tok, userName u)
         | (tok, uid) <- HM.toList toks
         , Just u <- [HM.lookup uid usrs]
         ]

-- | Apply an update to the users TVar AND persist @users.json@ after every
-- mutation (in-memory state for reads, JSON on disk for restarts — the same
-- pattern as the stats module). Session tokens are NOT persisted; API keys
-- are, inside each user.
modifyAuthUsers :: (HashMap Text User -> HashMap Text User) -> AppM (HashMap Text User)
modifyAuthUsers f = do
  cfg <- asks envConfig
  tv  <- usersTVar
  updated <- liftIO $ atomically $ do
    modifyTVar' tv f
    readTVar tv
  liftIO (saveUsers (usersFilePath cfg) updated)
  pure updated

-- | Module init: load (or bootstrap) @users.json@ into the "auth.users"
-- TVar, create the in-memory "auth.tokens" TVar, and stash both in the
-- typed registry.
initAuthState :: AppM ()
initAuthState = do
  cfg <- asks envConfig
  liftIO (createDirectoryIfMissing True (configStateDir cfg))
  users <- liftIO (loadOrBootstrapUsers cfg)
  tvU <- liftIO (newTVarIO users)
  putState authUsersKey tvU
  tvT <- liftIO (newTVarIO (HM.empty :: HashMap Text Text))
  putState authTokensKey tvT
  logInfo ("auth: " <> T.pack (show (HM.size users)) <> " user(s) loaded")

-- Internals ---------------------------------------------------------------

usersTVar :: AppM (TVar (HashMap Text User))
usersTVar = do
  m <- getState authUsersKey
  case m of
    Just tv -> pure tv
    Nothing -> throwError (InternalError (authUsersKey <> " not initialised"))

tokensTVar :: AppM (TVar (HashMap Text Text))
tokensTVar = do
  m <- getState authTokensKey
  case m of
    Just tv -> pure tv
    Nothing -> throwError (InternalError (authTokensKey <> " not initialised"))

currentUsers :: AppM (HashMap Text User)
currentUsers = liftIO . readTVarIO =<< usersTVar

currentTokens :: AppM (HashMap Text Text)
currentTokens = liftIO . readTVarIO =<< tokensTVar

-- | Next free numeric user id (ids are decimal strings; admin is \"1\").
nextUserId :: HashMap Text User -> Text
nextUserId users =
  let nums = catMaybes [readMaybe (T.unpack k) :: Maybe Int | k <- HM.keys users]
  in T.pack (show (maximum (0 : nums) + 1))
