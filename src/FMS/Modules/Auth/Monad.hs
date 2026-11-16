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
import Data.Time.Clock (getCurrentTime)
import Network.Wai (Request)
import System.Directory (createDirectoryIfMissing)
import Text.Read (readMaybe)

import FMS.Core.Monad (AppError (..), AppM, MonadLog (..), envConfig, getState, putState)
import FMS.Core.Router (tokenFromRequest)
import FMS.Core.Types.Config (Config (..))
import FMS.Core.Types.MediaItem (MediaItem (..))
import FMS.Core.Types.Policy
  ( UserPolicy
  , itemAllowedByPolicy
  , scheduleAllowsNow
  , policySchedules
  , emptyUserPolicy
  )
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
-- FMS.Modules.Metadata.Types is aeson-only, so importing it here cannot
-- create a cycle (FMS.Core.Query already does the same, milestone 4h).
import FMS.Modules.Metadata.Types (MediaMetadata)

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
  setPolicy :: Text -> UserPolicy -> m Bool            -- ^ userId, parental policy; False when unknown (milestone 9)
  setEnabled :: Text -> Bool -> m Bool                 -- ^ userId, enabled flag; False when unknown (milestone 9)

instance MonadAuth AppM where
  listUsers = do
    users <- currentUsers
    pure (sortOn userId (HM.elems users))

  getUser i = HM.lookup i <$> currentUsers

  createUser name pw admin = do
    users <- currentUsers
    let uid = nextUserId users
    h <- liftIO (hashPassword pw)
    let u = User uid name h admin [] Nothing emptyUserPolicy True
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
    -- milestone 9: disabled users lose their live tokens and api keys
    pure $ case HM.lookup tok toks of
      Just uid -> HM.lookup uid usrs >>= \u -> if userEnabled u then Just u else Nothing
      Nothing  -> find (\u -> userEnabled u && tok `elem` apiKeys u) (HM.elems usrs)

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

  setPolicy i pol = do
    m <- getUser i
    case m of
      Nothing -> pure False
      Just _  -> do
        _ <- modifyAuthUsers (HM.adjust (\u -> u { userPolicy = pol }) i)
        pure True

  setEnabled i en = do
    m <- getUser i
    case m of
      Nothing -> pure False
      Just _  -> do
        _ <- modifyAuthUsers (HM.adjust (\u -> u { userEnabled = en }) i)
        -- milestone 9 fix: disabling a user also drops their live session
        -- tokens, so the router gate cannot pass them as anonymous
        if not en
          then do
            tt <- tokensTVar
            liftIO (atomically (modifyTVar' tt (HM.filter (/= i))))
          else pure ()
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

-- | Milestone 9: the item metadata cache lives in the typed registry under
-- the literal key \"metadata.cache\" (importing FMS.Modules.Metadata.Monad
-- from here would create an import cycle; Library.Routes.hs uses the same
-- literal-key pattern for the same reason).
metadataCacheMap :: AppM (HashMap Text MediaMetadata)
metadataCacheMap = do
  mTv <- getState "metadata.cache" :: AppM (Maybe (TVar (HashMap Text MediaMetadata)))
  maybe (pure HM.empty) (liftIO . readTVarIO) mTv

-- | Milestone 5d + 9: the items visible to the request's user. Admins and
-- anonymous (open mode) see everything; a non-admin user sees only items
-- in their allowed folders, only inside their access schedule (when set),
-- and only items allowed by their parental policy (blocked tags / max
-- rating, checked against the metadata cache).
visibleItems :: Maybe User -> [MediaItem] -> AppM [MediaItem]
visibleItems mUser items = case mUser of
  Just u | not (isAdmin u) -> do
    metaMap <- metadataCacheMap
    now <- liftIO getCurrentTime
    if not (scheduleAllowsNow (policySchedules (userPolicy u)) now)
      then pure []
      else pure [ i | i <- items
                   , canAccessFolder (userAllowedFolders u) (itemFolder i)
                   , itemAllowedByPolicy (userPolicy u) (HM.lookup (itemId i) metaMap)
                   ]
  _ -> pure items

-- | 'getItem' plus the milestone-5d folder check and the milestone-9
-- policy/schedule checks: 404 when the item is unknown, 401 when the
-- request's user may not access the item's folder, 403 when access is
-- outside the user's schedule or the item is blocked by parental controls.
getItemWithAccess :: Request -> Text -> AppM (Maybe MediaItem)
getItemWithAccess req i = do
  mItem <- getItem i
  case mItem of
    Nothing   -> pure Nothing
    Just item -> do
      mUser <- currentUserFromReq req
      folderAccessOrThrow mUser (itemFolder item)
      case mUser of
        Just u | not (isAdmin u) -> do
          now <- liftIO getCurrentTime
          if not (scheduleAllowsNow (policySchedules (userPolicy u)) now)
            then throwError (Forbidden "access outside allowed schedule")
            else do
              metaMap <- metadataCacheMap
              if itemAllowedByPolicy (userPolicy u) (HM.lookup i metaMap)
                then pure (Just item)
                else throwError (Forbidden "item blocked by parental controls")
        _ -> pure (Just item)

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
