module FMS.Modules.Auth.Routes
  ( authRoutes
  , PermissionsRequest(..)
  , PolicyPatch(..)
  , applyPolicyPatch
  ) where

import Control.Applicative ((<|>))
import Control.Monad (join)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (FromJSON (..), ToJSON (..), Value(..), eitherDecode, encode, object, withObject, (.:), (.:?), (.!=), (.=))
import qualified Data.Aeson.KeyMap as KeyMap
import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types (ResponseHeaders, status201, status204)
import Network.Wai (Request, Response, responseLBS, strictRequestBody)

import FMS.Core.Module (Route, Segment (..), delete, get, jsonResponse, post)
import FMS.Core.Monad (AppError (..), AppM, envConfig)
import FMS.Core.Router (tokenFromRequest)
import FMS.Core.Types.Config (Config (..))
import FMS.Core.Types.Policy (AccessSchedule, UserPolicy (..))
import FMS.Core.Types.User (User (..), checkPasswordPolicy)
import FMS.Modules.Auth.Monad (MonadAuth (..), requireAdmin, verifyPassword)

-- | The users/auth/sessions API (docs/todo.org section 8,
-- Jellyfin-shaped). Order matters: the literal routes ([Users, Public],
-- [Users, AuthenticateByName]) must precede the [Users, Capture \"id\"]
-- routes so the literals win.
authRoutes :: [Route]
authRoutes =
  [ post   [Lit "Users", Lit "AuthenticateByName"] authenticateHandler
  , get    [Lit "Users", Lit "Public"]             publicUsersHandler
  , get    [Lit "Users"]                          listUsersHandler
  , post   [Lit "Users"]                          createUserHandler
  , get    [Lit "Users", Capture "id"]            getUserHandler
  , delete [Lit "Users", Capture "id"]            deleteUserHandler
  , post   [Lit "Users", Capture "id", Lit "Password"] passwordHandler
  , post   [Lit "Users", Capture "id", Lit "ApiKey"]   apiKeyHandler
  , get    [Lit "Users", Capture "id", Lit "Permissions"] getPermissionsHandler
  , post   [Lit "Users", Capture "id", Lit "Permissions"] setPermissionsHandler
  , get    [Lit "Users", Capture "id", Lit "Policy"]  getPolicyHandler
  , post   [Lit "Users", Capture "id", Lit "Policy"]  setPolicyHandler
  , post   [Lit "Users", Capture "id", Lit "Enable"]  (setEnabledHandler True)
  , post   [Lit "Users", Capture "id", Lit "Disable"] (setEnabledHandler False)
  ]

-- Request bodies ------------------------------------------------------------

-- | Body of @POST /Users/AuthenticateByName@ (Jellyfin field names).
data AuthRequest = AuthRequest
  { arUsername :: Text
  , arPw       :: Text
  }

instance FromJSON AuthRequest where
  parseJSON = withObject "AuthRequest" $ \o ->
    AuthRequest <$> o .: "Username" <*> o .: "Pw"

-- | Body of @POST /Users@.
data CreateUserRequest = CreateUserRequest
  { curName     :: Text
  , curPassword :: Text
  , curIsAdmin  :: Bool
  }

instance FromJSON CreateUserRequest where
  parseJSON = withObject "CreateUserRequest" $ \o ->
    CreateUserRequest
      <$> o .: "Name"
      <*> o .: "Password"
      <*> o .:? "IsAdmin" .!= False

-- | Body of @POST /Users/:id/Password@ (milestone 8b): @NewPw@ (alias
-- @NewPassword@) is required; @CurrentPw@ (alias @CurrentPassword@) is
-- optional at the JSON level and enforced by the handler for non-admin
-- self-changes.
data PasswordRequest = PasswordRequest
  { prNewPw     :: Text
  , prCurrentPw :: Maybe Text
  }

instance FromJSON PasswordRequest where
  parseJSON = withObject "PasswordRequest" $ \o -> do
    mNew  <- o .:? "NewPw"
    mNew' <- o .:? "NewPassword"
    newPw <- case mNew <|> mNew' of
      Just p  -> pure p
      Nothing -> fail "missing NewPw"
    mCur  <- o .:? "CurrentPw"
    mCur' <- o .:? "CurrentPassword"
    pure (PasswordRequest newPw (mCur <|> mCur'))

-- | Body of @POST /Users/:id/Permissions@ (milestone 5d):
-- @{\"AllowedFolders\": [\"1\", ...] | null}@. A missing or null value
-- means "all folders" ('Nothing').
data PermissionsRequest = PermissionsRequest
  { prAllowedFolders :: Maybe [Text]
  } deriving (Eq, Show)

instance FromJSON PermissionsRequest where
  parseJSON = withObject "PermissionsRequest" $ \o ->
    PermissionsRequest <$> (join <$> o .:? "AllowedFolders" .!= Nothing)

instance ToJSON PermissionsRequest where
  toJSON pr = object ["AllowedFolders" .= prAllowedFolders pr]

-- | The permissions response shape (also used by GET):
-- @{\"AllowedFolders\": [...]|null, \"IsAdmin\": bool}@.
permissionsResponse :: User -> Value
permissionsResponse u = object
  [ "AllowedFolders" .= userAllowedFolders u
  , "IsAdmin"        .= isAdmin u
  ]

-- | Body of @POST /Users/:id/Policy@ (milestone 9; @MaxStreamingBitrate@
-- added in milestone 6f). All fields optional; absent fields keep the
-- current value, explicit null clears a scalar (@MaxParentalRating@ /
-- @MaxStreamingBitrate@ -> 'Nothing').
data PolicyPatch = PolicyPatch
  { ppMaxRating   :: Maybe (Maybe Text)   -- ^ Nothing = absent; Just Nothing = clear
  , ppBlockedTags :: Maybe [Text]
  , ppSchedules   :: Maybe [AccessSchedule]
  , ppMaxBitrate  :: Maybe (Maybe Int)     -- ^ milestone 6f; Nothing = absent; Just Nothing = clear
  } deriving (Eq, Show)

instance FromJSON PolicyPatch where
  parseJSON = withObject "PolicyPatch" $ \o -> do
    -- aeson >= 2.2's .:? collapses an explicit null to "absent", so the
    -- scalar fields are read via KeyMap.lookup to keep the three states
    -- absent / null (clear) / value distinct.
    mr <- case KeyMap.lookup "MaxParentalRating" o of
      Nothing   -> pure Nothing
      Just Null -> pure (Just Nothing)
      Just v    -> Just . Just <$> parseJSON v
    br <- case KeyMap.lookup "MaxStreamingBitrate" o of
      Nothing   -> pure Nothing
      Just Null -> pure (Just Nothing)
      Just v    -> Just . Just <$> parseJSON v
    PolicyPatch
      <$> pure mr
      <*> o .:? "BlockedTags"
      <*> o .:? "AccessSchedules"
      <*> pure br

-- | Merge a 'PolicyPatch' over an existing 'UserPolicy' (pure, exported for
-- tests).
applyPolicyPatch :: UserPolicy -> PolicyPatch -> UserPolicy
applyPolicyPatch p pp = UserPolicy
  { policyMaxRating   = case ppMaxRating pp of
      Nothing  -> policyMaxRating p
      Just mr  -> mr
  , policyBlockedTags = fromMaybe (policyBlockedTags p) (ppBlockedTags pp)
  , policySchedules   = fromMaybe (policySchedules p) (ppSchedules pp)
  , policyMaxStreamingBitrate = case ppMaxBitrate pp of
      Nothing  -> policyMaxStreamingBitrate p
      Just mb  -> mb
  }

-- Handlers ------------------------------------------------------------------

-- | @POST /Users/AuthenticateByName@ with @{\"Username\":\"...\",\"Pw\":\"...\"\}@.
-- 200 @{\"AccessToken\":\"...\",\"User\":{...}}@ on success (the User JSON is
-- the public shape — no passwordHash), 401 on bad credentials, 403 when the
-- account is disabled (milestone 9), 400 on a malformed body.
authenticateHandler :: [(Text, Text)] -> Request -> AppM Response
authenticateHandler _captures req = do
  ar <- parseBody req
  -- milestone 9: a disabled account gets a distinct 403, not a generic 401
  users <- listUsers
  case find (\u -> T.toLower (userName u) == T.toLower (arUsername ar)) users of
    Just u | not (userEnabled u) -> throwError (Forbidden "user is disabled")
    _ -> pure ()
  m  <- authenticate (arUsername ar) (arPw ar)
  case m of
    Nothing -> throwError (Unauthorized "invalid username or password")
    Just (u, tok) ->
      pure (jsonResponse (object ["AccessToken" .= tok, "User" .= u]))

-- | @GET /Users/Public@ — @[{\"Id\":\"...\",\"Name\":\"...\"}, ...]@; open
-- even in strict mode (it is in the router's open list).
publicUsersHandler :: [(Text, Text)] -> Request -> AppM Response
publicUsersHandler _captures _req = do
  users <- listUsers
  pure (jsonResponse [object ["Id" .= userId u, "Name" .= userName u] | u <- users])

-- | @GET /Users@ — full user list (public JSON shape). Admin only.
listUsersHandler :: [(Text, Text)] -> Request -> AppM Response
listUsersHandler _captures req = do
  requireAdmin req
  jsonResponse <$> listUsers

-- | @POST /Users@ with @{\"Name\":\"...\",\"Password\":\"...\",\"IsAdmin\"?:bool}@.
-- Admin only; 201 with the created user's public JSON. The password must
-- satisfy the configured minimum length (milestone 8b) -> 400 otherwise.
createUserHandler :: [(Text, Text)] -> Request -> AppM Response
createUserHandler _captures req = do
  requireAdmin req
  cur <- parseBody req
  cfg <- asks envConfig
  case checkPasswordPolicy (configPasswordMinLength cfg) (curPassword cur) of
    Left e  -> throwError (BadRequest e)
    Right _ -> pure ()
  u <- createUser (curName cur) (curPassword cur) (curIsAdmin cur)
  pure (responseLBS status201 jsonCT (encode u))

-- | @GET /Users/:id@ — one user, self or admin.
getUserHandler :: [(Text, Text)] -> Request -> AppM Response
getUserHandler captures req = do
  i <- captureId captures
  requireSelfOrAdmin req i
  m <- getUser i
  case m of
    Nothing -> throwError (NotFound ("no user with id " <> i))
    Just u  -> pure (jsonResponse u)

-- | @DELETE /Users/:id@ — admin only; 204 on success, 404 when missing.
deleteUserHandler :: [(Text, Text)] -> Request -> AppM Response
deleteUserHandler captures req = do
  requireAdmin req
  i <- captureId captures
  m <- getUser i
  case m of
    Nothing -> throwError (NotFound ("no user with id " <> i))
    Just _  -> do
      deleteUser i
      pure (responseLBS status204 [] "")

-- | @POST /Users/:id/Password@ with @{\"NewPw\":\"...\",\"CurrentPw\"?:\"...\"}@
-- (aliases @NewPassword@/@CurrentPassword@) — self or admin; responds
-- @{\"status\":\"ok\"}@. Milestone 8b: the new password must satisfy the
-- configured minimum length (400); a NON-ADMIN changing their OWN
-- password must supply the correct @CurrentPw@ (403 otherwise); when an
-- ADMIN changes ANOTHER user's password the target's @MustChangePassword@
-- flag is set (forced reset), and cleared on a self-change.
passwordHandler :: [(Text, Text)] -> Request -> AppM Response
passwordHandler captures req = do
  i <- captureId captures
  requireSelfOrAdmin req i
  u <- requireExistingUser i
  pr <- parseBody req
  cfg <- asks envConfig
  case checkPasswordPolicy (configPasswordMinLength cfg) (prNewPw pr) of
    Left e  -> throwError (BadRequest e)
    Right _ -> pure ()
  mCaller <- currentUser req
  let callerIsSelf  = maybe False (\c -> userId c == i) mCaller
      callerIsAdmin = maybe False isAdmin mCaller
  -- non-admin self-change: verify the current password (403 on mismatch)
  if callerIsSelf && not callerIsAdmin
    then case prCurrentPw pr of
      Just cur | verifyPassword (passwordHash u) cur -> pure ()
      _ -> throwError (Forbidden "current password required or incorrect")
    else pure ()
  setPassword i (prNewPw pr)
  _ <- setMustChangePassword i (callerIsAdmin && not callerIsSelf)
  pure (jsonResponse (object ["status" .= ("ok" :: Text)]))

-- | @POST /Users/:id/ApiKey@ — self or admin; mints and persists a new API
-- key, responds @{\"ApiKey\":\"...\"}@.
apiKeyHandler :: [(Text, Text)] -> Request -> AppM Response
apiKeyHandler captures req = do
  i <- captureId captures
  requireSelfOrAdmin req i
  _ <- requireExistingUser i
  tok <- addApiKey i
  pure (jsonResponse (object ["ApiKey" .= tok]))

-- | @GET /Users/:id/Permissions@ — self or admin; 200
-- @{\"AllowedFolders\": [...]|null, \"IsAdmin\": bool}@, 404 unknown user.
getPermissionsHandler :: [(Text, Text)] -> Request -> AppM Response
getPermissionsHandler captures req = do
  i <- captureId captures
  requireSelfOrAdmin req i
  u <- requireExistingUser i
  pure (jsonResponse (permissionsResponse u))

-- | @POST /Users/:id/Permissions@ with @{\"AllowedFolders\": [...]|null}@ —
-- self or admin (admin required for other users); persists to users.json,
-- 200 with the updated permissions, 404 unknown user, 400 bad JSON.
setPermissionsHandler :: [(Text, Text)] -> Request -> AppM Response
setPermissionsHandler captures req = do
  i <- captureId captures
  -- milestone 16: admin-only; a non-admin must not change anyone's folder
  -- permissions (previously self-or-admin, which allowed self-escalation
  -- of AllowedFolders to null)
  requireAdmin req
  _ <- requireExistingUser i
  pr <- parseBody req
  _ <- setAllowedFolders i (prAllowedFolders pr)
  u' <- requireExistingUser i
  pure (jsonResponse (permissionsResponse u'))

-- | @GET /Users/:id/Policy@ — self or admin; 200 with the user's
-- 'UserPolicy' JSON (@MaxParentalRating@/@BlockedTags@/@AccessSchedules@),
-- 404 unknown user, 403 wrong user (milestone 9).
getPolicyHandler :: [(Text, Text)] -> Request -> AppM Response
getPolicyHandler captures req = do
  i <- captureId captures
  requireSelfOrAdmin403 req i
  u <- requireExistingUser i
  pure (jsonResponse (userPolicy u))

-- | @POST /Users/:id/Policy@ with a partial body
-- @{\"MaxParentalRating\"?: ...|null, \"BlockedTags\"?: [...], \"AccessSchedules\"?: [...]}@
-- merged over the current policy — self or admin; 200 with the updated
-- policy, 404 unknown user, 400 bad JSON (milestone 9).
setPolicyHandler :: [(Text, Text)] -> Request -> AppM Response
setPolicyHandler captures req = do
  i <- captureId captures
  requireSelfOrAdmin403 req i
  u <- requireExistingUser i
  pp <- parseBody req
  let pol = applyPolicyPatch (userPolicy u) pp
  _ <- setPolicy i pol
  pure (jsonResponse pol)

-- | @POST /Users/:id/Enable@ / @POST /Users/:id/Disable@ — self or admin
-- (admin required for other users); 200 @{\"enabled\": bool}@, 404 unknown
-- user (milestone 9).
setEnabledHandler :: Bool -> [(Text, Text)] -> Request -> AppM Response
setEnabledHandler en captures req = do
  i <- captureId captures
  requireSelfOrAdmin403 req i
  _ <- requireExistingUser i
  _ <- setEnabled i en
  pure (jsonResponse (object ["enabled" .= en]))

-- NOTE: @GET /Sessions@ used to live here (listing live auth tokens);
-- milestone 6c moves it to "FMS.Modules.Sessions", where it returns the
-- Jellyfin-shaped active playback sessions instead.

-- Helpers -------------------------------------------------------------------

-- | Decode a JSON request body; 400 on invalid JSON.
parseBody :: FromJSON a => Request -> AppM a
parseBody req = do
  body <- liftIO (strictRequestBody req)
  case eitherDecode body of
    Left err -> throwError (BadRequest ("invalid JSON body: " <> T.pack err))
    Right v  -> pure v

captureId :: [(Text, Text)] -> AppM Text
captureId captures =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just i  -> pure i

requireExistingUser :: Text -> AppM User
requireExistingUser i = do
  m <- getUser i
  case m of
    Nothing -> throwError (NotFound ("no user with id " <> i))
    Just u  -> pure u

-- | The user behind the request's credential, if any. Works in both modes:
-- in strict mode the router gate has already validated the token, in open
-- mode a missing token simply means anonymous.
currentUser :: Request -> AppM (Maybe User)
currentUser req =
  case tokenFromRequest req of
    Nothing  -> pure Nothing
    Just tok -> resolveToken tok

-- | Milestone 16: 'requireAdmin' moved to "FMS.Modules.Auth.Monad" (shared
-- by all modules with admin-only routes) with stricter semantics: no token
-- or non-admin -> 403 even in open mode.

-- | Milestone 9: like 'requireSelfOrAdmin', but a present non-matching
-- credential gets 403 Forbidden (used by the Policy/Enable/Disable routes,
-- where the task specifies 403 for a wrong user).
requireSelfOrAdmin403 :: Request -> Text -> AppM ()
requireSelfOrAdmin403 req targetId = do
  cfg <- asks envConfig
  mUser <- currentUser req
  case mUser of
    Just u | isAdmin u || userId u == targetId -> pure ()
    Just _  -> throwError (Forbidden "self or admin required")
    Nothing
      | configRequireAuth cfg -> throwError (Unauthorized "authentication required")
      | otherwise             -> pure ()

-- | Self-or-admin gate for @/Users/:id/...@.
requireSelfOrAdmin :: Request -> Text -> AppM ()
requireSelfOrAdmin req targetId = do
  cfg <- asks envConfig
  mUser <- currentUser req
  case mUser of
    Just u | isAdmin u || userId u == targetId -> pure ()
    Just _  -> throwError (Unauthorized "self or admin required")
    Nothing
      | configRequireAuth cfg -> throwError (Unauthorized "authentication required")
      | otherwise             -> pure ()

jsonCT :: ResponseHeaders
jsonCT = [("Content-Type", "application/json")]
