module FMS.Modules.Auth.Monad
  ( MonadAuth (..)
  , authUsersKey
  , authTokensKey
  , initAuthState
  , modifyAuthUsers
  , hashPassword
  , verifyPassword
  , mintToken
  , canAccessFolder
  , allowedFoldersFor
  , currentUserFromReq
  , folderAccessOrThrow
  , visibleItems
  , getItemWithAccess
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
import Network.Wai (Request)
import System.Directory (createDirectoryIfMissing)
import Text.Read (readMaybe)

import FMS.Core.Monad (AppError (..), AppM, MonadLog (..), envConfig, getState, putState)
import FMS.Core.Router (tokenFromRequest)
import FMS.Core.Types.Config (Config (..))
import FMS.Core.Types.MediaItem (MediaItem (..))
import FMS.Core.Types.User (User (..))
import FMS.Modules.Auth.State
  ( hashPassword
  , loadOrBootstrapUsers
  , mintToken
  , saveUsers
  , usersFilePath
  , verifyPassword
  )
import FMS.Modules.Library.Monad (MonadLibrary (..))

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
  setAllowedFolders :: Text -> Maybe [Text] -> m Bool  -- ^ userId, folders (Nothing = all); False when unknown (milestone 5d)

instance MonadAuth AppM where
  listUsers = do
    users <- currentUsers
    pure (sortOn userId (HM.elems users))

  getUser i = HM.lookup i <$> currentUsers

  createUser name pw admin = do
    users <- currentUsers
    let uid = nextUserId users
    h <- liftIO (hashPassword pw)
    let u = User uid name h admin [] Nothing
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

  setAllowedFolders i mFolders = do
    m <- getUser i
    case m of
      Nothing -> pure False
      Just _  -> do
        _ <- modifyAuthUsers (HM.adjust (\u -> u { userAllowedFolders = mFolders }) i)
        pure True

-- | Pure (milestone 5d): the folder restriction of a user
-- ('Nothing' = all folders).
allowedFoldersFor :: User -> Maybe [Text]
allowedFoldersFor = userAllowedFolders

-- | Pure (milestone 5d): may a user with this folder restriction access
-- @folderId@? 'Nothing' = all folders allowed.
canAccessFolder :: Maybe [Text] -> Text -> Bool
canAccessFolder Nothing  _       = True
canAccessFolder (Just ids) fid   = fid `elem` ids

-- | The user behind the request's credential, if any (session token or
-- API key). 'Nothing' means anonymous / open mode.
currentUserFromReq :: Request -> AppM (Maybe User)
currentUserFromReq req =
  case tokenFromRequest req of
    Nothing  -> pure Nothing
    Just tok -> resolveToken tok

-- | Milestone 5d enforcement: admins pass, users with 'Nothing'
-- allowedFolders pass, users with a restriction pass only for their
-- folders -- anything else is 401. Anonymous (no/invalid token) passes:
-- in open mode there is no auth at all, in strict mode the router gate
-- has already rejected the request.
folderAccessOrThrow :: Maybe User -> Text -> AppM ()
folderAccessOrThrow mUser folderId =
  case mUser of
    Just u
      | isAdmin u -> pure ()
      | canAccessFolder (userAllowedFolders u) folderId -> pure ()
      | otherwise -> throwError (Unauthorized ("folder access denied: " <> folderId))
    Nothing -> pure ()

-- | Milestone 5d: the items visible to the request's user. Admins and
-- anonymous (open mode) see everything; a restricted user sees only items
-- in their allowed folders.
visibleItems :: Maybe User -> [MediaItem] -> [MediaItem]
visibleItems mUser = case mUser of
  Just u | not (isAdmin u) -> filter (canAccessFolder (userAllowedFolders u) . itemFolder)
  _                        -> id

-- | 'getItem' plus the milestone-5d folder check: 404 when the item is
-- unknown, 401 when the request's user may not access the item's folder.
getItemWithAccess :: Request -> Text -> AppM (Maybe MediaItem)
getItemWithAccess req i = do
  mItem <- getItem i
  case mItem of
    Nothing   -> pure Nothing
    Just item -> do
      mUser <- currentUserFromReq req
      folderAccessOrThrow mUser (itemFolder item)
      pure (Just item)

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
