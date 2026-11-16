module FMS.Modules.Auth.Routes
  ( authRoutes
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (FromJSON (..), eitherDecode, encode, object, withObject, (.:), (.:?), (.!=), (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types (ResponseHeaders, status201, status204)
import Network.Wai (Request, Response, responseLBS, strictRequestBody)

import FMS.Core.Module (Route, Segment (..), delete, get, jsonResponse, post)
import FMS.Core.Monad (AppError (..), AppM, envConfig)
import FMS.Core.Router (tokenFromRequest)
import FMS.Core.Types.Config (Config (..))
import FMS.Core.Types.User (User (..))
import FMS.Modules.Auth.Monad (MonadAuth (..))

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

-- | Body of @POST /Users/:id/Password@.
data PasswordRequest = PasswordRequest
  { prNewPw :: Text
  }

instance FromJSON PasswordRequest where
  parseJSON = withObject "PasswordRequest" $ \o ->
    PasswordRequest <$> o .: "NewPw"

-- Handlers ------------------------------------------------------------------

-- | @POST /Users/AuthenticateByName@ with @{\"Username\":\"...\",\"Pw\":\"...\"\}@.
-- 200 @{\"AccessToken\":\"...\",\"User\":{...}}@ on success (the User JSON is
-- the public shape — no passwordHash), 401 on bad credentials, 400 on a
-- malformed body.
authenticateHandler :: [(Text, Text)] -> Request -> AppM Response
authenticateHandler _captures req = do
  ar <- parseBody req
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
-- Admin only; 201 with the created user's public JSON.
createUserHandler :: [(Text, Text)] -> Request -> AppM Response
createUserHandler _captures req = do
  requireAdmin req
  cur <- parseBody req
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

-- | @POST /Users/:id/Password@ with @{\"NewPw\":\"...\"}@ — self or admin;
-- responds @{\"status\":\"ok\"}@.
passwordHandler :: [(Text, Text)] -> Request -> AppM Response
passwordHandler captures req = do
  i <- captureId captures
  requireSelfOrAdmin req i
  _ <- requireExistingUser i
  pr <- parseBody req
  setPassword i (prNewPw pr)
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

-- | Admin gate. A present credential must belong to an admin (401
-- otherwise); when no credential is present at all, the request is allowed
-- only in open mode (configRequireAuth off) — in strict mode the router
-- gate has already rejected it.
requireAdmin :: Request -> AppM ()
requireAdmin req = do
  cfg <- asks envConfig
  mUser <- currentUser req
  case mUser of
    Just u | isAdmin u -> pure ()
    Just _  -> throwError (Unauthorized "admin privileges required")
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
