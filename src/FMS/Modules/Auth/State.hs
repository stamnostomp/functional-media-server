module FMS.Modules.Auth.State
  ( usersFilePath
  , loadUsers
  , saveUsers
  , loadOrBootstrapUsers
  , hashPassword
  , verifyPassword
  , mintToken
  ) where

import Control.Exception (SomeException, try)
import Control.Monad (when)
import Crypto.BCrypt (hashPasswordUsingPolicy, slowerBcryptHashingPolicy, validatePassword)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.Random (newStdGen, randomRs)

import FMS.Core.Types.Config (Config (..))
import FMS.Core.Types.Policy (emptyUserPolicy)
import FMS.Core.Types.User (User (..), toInternalJSON)

-- | Where the user map is persisted: @stateDir/users.json@ — a bare JSON
-- object mapping user id -> 'User' (internal, full JSON incl. the password
-- hash; the public 'ToJSON' never leaks it).
usersFilePath :: Config -> FilePath
usersFilePath cfg = configStateDir cfg </> "users.json"

-- | Load the persisted users file. A missing or unreadable file yields an
-- empty map — like the stats persistence, this is best-effort, never fatal.
loadUsers :: FilePath -> IO (HashMap Text User)
loadUsers path = do
  exists <- doesFileExist path
  if not exists
    then pure HM.empty
    else do
      result <- try (LBS.readFile path) :: IO (Either SomeException LBS.ByteString)
      pure $ case result of
        Left _      -> HM.empty
        Right bytes -> fromMaybe HM.empty (Aeson.decode bytes)

-- | Persist the whole user map as a bare JSON object (internal shape, so
-- the password hash survives restarts).
saveUsers :: FilePath -> HashMap Text User -> IO ()
saveUsers path users = LBS.writeFile path (Aeson.encode (HM.map toInternalJSON users))

-- | Load @users.json@, or — on first run, when the file is missing —
-- bootstrap the admin account: id \"1\", name \"admin\", password from
-- @FMS_ADMIN_PASSWORD@ (default \"admin\", with a logged warning), one API
-- key. The freshly created map is saved and returned.
loadOrBootstrapUsers :: Config -> IO (HashMap Text User)
loadOrBootstrapUsers cfg = do
  let path = usersFilePath cfg
  exists <- doesFileExist path
  if exists
    then loadUsers path
    else do
      mPw <- lookupEnv "FMS_ADMIN_PASSWORD"
      let pw = T.pack (fromMaybe "admin" mPw)
      when (isNothing mPw) $
        hPutStrLn stderr "[WARN] FMS_ADMIN_PASSWORD not set: bootstrap admin password is \"admin\" -- set FMS_ADMIN_PASSWORD or change it via POST /Users/1/Password"
      h   <- hashPassword pw
      key <- mintToken
      let admin = User "1" "admin" h True [key] Nothing emptyUserPolicy True False
          users = HM.singleton "1" admin
      saveUsers path users
      pure users

-- | Hash a plaintext password with bcrypt. Uses the package's default
-- (slower, cost-14) policy — deliberately slowish, so callers run it via
-- 'liftIO' and tests keep the op count small.
hashPassword :: Text -> IO Text
hashPassword pw = do
  mh <- hashPasswordUsingPolicy slowerBcryptHashingPolicy (TE.encodeUtf8 pw)
  pure (maybe "" TE.decodeUtf8 mh)

-- | Verify a plaintext password against a bcrypt hash. Note the bcrypt
-- package argument order: first argument is the HASH, second the password
-- attempt.
verifyPassword :: Text -> Text -> Bool
verifyPassword hash pw = validatePassword (TE.encodeUtf8 hash) (TE.encodeUtf8 pw)

-- | A fresh random credential (session token / API key): 32 hex chars.
mintToken :: IO Text
mintToken = do
  g <- newStdGen
  let rs = take 32 (randomRs (0, 15) g) :: [Int]
  pure (T.pack (map (\r -> "0123456789abcdef" !! r) rs))
