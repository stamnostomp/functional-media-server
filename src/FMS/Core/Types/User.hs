module FMS.Core.Types.User
  ( User(..)
  , toInternalJSON
  , checkPasswordPolicy
  ) where

import Control.Monad (join)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, (.:), (.:?), (.!=), (.=))
import Data.Text (Text)
import qualified Data.Text as T

import FMS.Core.Types.Policy (UserPolicy, emptyUserPolicy)

-- | A user account (docs/todo.org section 8, Jellyfin-style).
--
-- JSON comes in two shapes:
--
-- * /internal/ — all fields ('FromJSON' instance + 'toInternalJSON'),
--   used only for persistence in @stateDir\/users.json@;
-- * /public/ — the 'ToJSON' instance used by the HTTP API, which never
--   leaks the password hash.
--
-- Milestone 5d (per-folder permissions) added 'userAllowedFolders':
-- 'Nothing' means access to ALL media folders, @Just ids@ restricts to
-- those folder ids. 'FromJSON' defaults a missing (or null) field to
-- 'Nothing', so pre-5d @users.json@ files keep loading.
--
-- Milestone 9 (parental controls + enable/disable) added 'userPolicy'
-- (defaults to 'emptyUserPolicy') and 'userEnabled' (defaults to 'True'),
-- so pre-9 @users.json@ files keep loading.
--
-- Milestone 8b (password policy) added 'userMustChangePassword'
-- (defaults to 'False'), set to 'True' when an admin resets another
-- user's password, so the user is forced to pick a new one.
data User = User
  { userId       :: !Text
  , userName     :: !Text
  , passwordHash :: !Text
  , isAdmin      :: !Bool
  , apiKeys      :: ![Text]
  , userAllowedFolders :: !(Maybe [Text])  -- ^ milestone 5d; Nothing = all folders
  , userPolicy   :: !UserPolicy             -- ^ milestone 9; emptyUserPolicy = no restrictions
  , userEnabled  :: !Bool                   -- ^ milestone 9; False = login rejected with 403
  , userMustChangePassword :: !Bool         -- ^ milestone 8b; True = forced reset
  } deriving (Eq, Show)

-- | Pure password-policy check (milestone 8b): @minLen <= 0@ disables the
-- policy (always 'Right'); otherwise the password must be at least
-- @minLen@ characters long (character count, not bytes) or the result is
-- a 'Left' with an ASCII error message the routes turn into a 400.
checkPasswordPolicy :: Int -> Text -> Either Text ()
checkPasswordPolicy minLen pw
  | minLen <= 0 = Right ()
  | length (T.unpack pw) >= minLen = Right ()
  | otherwise = Left ("password must be at least " <> T.pack (show minLen)
                      <> " characters")

-- | Internal (persistence) JSON: all fields, including the password hash.
-- The @allowedFolders@ key is optional and defaults to null/Nothing, so
-- old users.json files (written before milestone 5d) still decode.
-- @policy@ and @enabled@ (milestone 9) default likewise, so pre-9 files load.
instance FromJSON User where
  parseJSON = withObject "User" $ \o ->
    User
      <$> o .: "userId"
      <*> o .: "userName"
      <*> o .: "passwordHash"
      <*> o .: "isAdmin"
      <*> o .: "apiKeys"
      <*> (join <$> o .:? "allowedFolders" .!= Nothing)
      <*> ((join <$> o .:? "policy") .!= emptyUserPolicy)
      <*> (o .:? "enabled" .!= True)
      <*> (o .:? "mustChangePassword" .!= False)

-- | Public API JSON: no password hash, just whether one is set. Includes
-- the per-folder access list (milestone 5d) as @AllowedFolders@.
instance ToJSON User where
  toJSON u = object
    [ "id"            .= userId u
    , "name"          .= userName u
    , "isAdmin"       .= isAdmin u
    , "hasPassword"   .= not (T.null (passwordHash u))
    , "AllowedFolders" .= userAllowedFolders u
    , "enabled"       .= userEnabled u          -- milestone 9
    , "Policy"        .= userPolicy u           -- milestone 9
    , "MustChangePassword" .= userMustChangePassword u  -- milestone 8b
    ]

-- | Internal full JSON (all fields, incl. the password hash) — used by the
-- persistence layer in "FMS.Modules.Auth.State" so that @users.json@ keeps
-- the hash while API responses never see it.
toInternalJSON :: User -> Value
toInternalJSON u = object
  [ "userId"         .= userId u
  , "userName"       .= userName u
  , "passwordHash"   .= passwordHash u
  , "isAdmin"        .= isAdmin u
  , "apiKeys"        .= apiKeys u
  , "allowedFolders" .= userAllowedFolders u
  , "policy"         .= userPolicy u
  , "enabled"        .= userEnabled u
  , "mustChangePassword" .= userMustChangePassword u
  ]
