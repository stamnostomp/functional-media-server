module FMS.Core.Types.User
  ( User(..)
  , toInternalJSON
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, (.:), (.=))
import Data.Text (Text)
import qualified Data.Text as T

-- | A user account (docs/todo.org section 8, Jellyfin-style).
--
-- JSON comes in two shapes:
--
-- * /internal/ — all fields ('FromJSON' instance + 'toInternalJSON'),
--   used only for persistence in @stateDir\/users.json@;
-- * /public/ — the 'ToJSON' instance used by the HTTP API, which never
--   leaks the password hash: @{id, name, isAdmin, hasPassword}@.
data User = User
  { userId       :: !Text
  , userName     :: !Text
  , passwordHash :: !Text
  , isAdmin      :: !Bool
  , apiKeys      :: ![Text]
  } deriving (Eq, Show)

-- | Internal (persistence) JSON: all fields, including the password hash.
instance FromJSON User where
  parseJSON = withObject "User" $ \o ->
    User
      <$> o .: "userId"
      <*> o .: "userName"
      <*> o .: "passwordHash"
      <*> o .: "isAdmin"
      <*> o .: "apiKeys"

-- | Public API JSON: no password hash, just whether one is set.
instance ToJSON User where
  toJSON u = object
    [ "id"          .= userId u
    , "name"        .= userName u
    , "isAdmin"     .= isAdmin u
    , "hasPassword" .= not (T.null (passwordHash u))
    ]

-- | Internal full JSON (all fields, incl. the password hash) — used by the
-- persistence layer in "FMS.Modules.Auth.State" so that @users.json@ keeps
-- the hash while API responses never see it.
toInternalJSON :: User -> Value
toInternalJSON u = object
  [ "userId"       .= userId u
  , "userName"     .= userName u
  , "passwordHash" .= passwordHash u
  , "isAdmin"      .= isAdmin u
  , "apiKeys"      .= apiKeys u
  ]
