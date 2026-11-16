module FMS.Core.Types.User
  ( User(..)
  , toInternalJSON
  ) where

import Control.Monad (join)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, (.:), (.:?), (.!=), (.=))
import Data.Text (Text)
import qualified Data.Text as T

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
data User = User
  { userId       :: !Text
  , userName     :: !Text
  , passwordHash :: !Text
  , isAdmin      :: !Bool
  , apiKeys      :: ![Text]
  , userAllowedFolders :: !(Maybe [Text])  -- ^ milestone 5d; Nothing = all folders
  } deriving (Eq, Show)

-- | Internal (persistence) JSON: all fields, including the password hash.
-- The @allowedFolders@ key is optional and defaults to null/Nothing, so
-- old users.json files (written before milestone 5d) still decode.
instance FromJSON User where
  parseJSON = withObject "User" $ \o ->
    User
      <$> o .: "userId"
      <*> o .: "userName"
      <*> o .: "passwordHash"
      <*> o .: "isAdmin"
      <*> o .: "apiKeys"
      <*> (join <$> o .:? "allowedFolders" .!= Nothing)

-- | Public API JSON: no password hash, just whether one is set. Includes
-- the per-folder access list (milestone 5d) as @AllowedFolders@.
instance ToJSON User where
  toJSON u = object
    [ "id"            .= userId u
    , "name"          .= userName u
    , "isAdmin"       .= isAdmin u
    , "hasPassword"   .= not (T.null (passwordHash u))
    , "AllowedFolders" .= userAllowedFolders u
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
  ]
