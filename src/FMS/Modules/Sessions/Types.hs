module FMS.Modules.Sessions.Types
  ( PlaySession(..)
  , SessionCapabilities(..)
  , emptyCapabilities
  , applyCapabilities
  , validCommandName
  , latestSessionForToken
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.!=), (.=))
import Data.Char (ord)
import Data.List (sortOn)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)

-- | One live playback session (docs/todo.org section 6, milestone 6c):
-- a client reporting @POST /Sessions/Playing*@ for an item. Sessions
-- live in memory only (a TVar in the typed registry, key
-- \"sessions.active\"); they are not persisted across restarts.
--
-- The capability fields (casting/session capabilities, todo Chromecast/
-- AirPlay section) are all optional with defaults, so session JSON
-- without them still decodes. 'psToken' is internal bookkeeping (the
-- auth token that created the session, used to find the session's
-- websocket) and is NEVER serialized.
data PlaySession = PlaySession
  { psId              :: !Text          -- ^ session id (minted server-side)
  , psItemId          :: !Text          -- ^ library item being played ("" for a capabilities-only session)
  , psUserId          :: !(Maybe Text)  -- ^ user, when the request carried a valid token
  , psUserName        :: !(Maybe Text)
  , psPositionSeconds :: !Double        -- ^ last reported position
  , psIsPaused        :: !Bool
  , psStartedAt       :: !UTCTime
  , psLastPingAt      :: !UTCTime       -- ^ last Playing/Ping/Progress report
  , psPlayableMediaTypes           :: ![Text]       -- ^ capability: PlayableMediaTypes
  , psSupportedCommands            :: ![Text]       -- ^ capability: SupportedCommands
  , psSupportsMediaControl         :: !Bool         -- ^ capability: SupportsMediaControl
  , psSupportsPersistentIdentifier :: !Bool         -- ^ capability: SupportsPersistentIdentifier
  , psDeviceId                     :: !(Maybe Text) -- ^ capability: DeviceId
  , psLastCommand                  :: !(Maybe Text) -- ^ last GeneralCommand name sent to this session
  , psToken                        :: !(Maybe Text) -- ^ internal: creating auth token (not in API JSON)
  } deriving (Eq, Show)

-- | API JSON with Jellyfin-style PascalCase keys. The capability fields
-- are emitted only when set (non-empty list / True / Just), so a session
-- without capabilities serializes exactly like the pre-capability shape.
-- 'psToken' is deliberately not emitted.
instance ToJSON PlaySession where
  toJSON s = object $
    [ "Id"              .= psId s
    , "ItemId"          .= psItemId s
    , "UserId"          .= psUserId s
    , "UserName"        .= psUserName s
    , "PositionSeconds" .= psPositionSeconds s
    , "IsPaused"        .= psIsPaused s
    , "StartedAt"       .= psStartedAt s
    , "LastPingAt"      .= psLastPingAt s
    ] ++
    [ "PlayableMediaTypes" .= psPlayableMediaTypes s | not (null (psPlayableMediaTypes s)) ] ++
    [ "SupportedCommands" .= psSupportedCommands s | not (null (psSupportedCommands s)) ] ++
    [ "SupportsMediaControl" .= psSupportsMediaControl s | psSupportsMediaControl s ] ++
    [ "SupportsPersistentIdentifier" .= psSupportsPersistentIdentifier s | psSupportsPersistentIdentifier s ] ++
    [ "DeviceId" .= d | Just d <- [psDeviceId s] ] ++
    [ "LastCommand" .= c | Just c <- [psLastCommand s] ]

-- | Decoding is fully backward compatible: every capability field is
-- optional and defaults (old session JSON without them decodes fine).
instance FromJSON PlaySession where
  parseJSON = withObject "PlaySession" $ \o ->
    PlaySession
      <$> o .:  "Id"
      <*> o .:  "ItemId"
      <*> o .:? "UserId"
      <*> o .:? "UserName"
      <*> o .:  "PositionSeconds"
      <*> o .:? "IsPaused" .!= False
      <*> o .:  "StartedAt"
      <*> o .:  "LastPingAt"
      <*> o .:? "PlayableMediaTypes" .!= []
      <*> o .:? "SupportedCommands" .!= []
      <*> o .:? "SupportsMediaControl" .!= False
      <*> o .:? "SupportsPersistentIdentifier" .!= False
      <*> o .:? "DeviceId"
      <*> o .:? "LastCommand"
      <*> pure Nothing   -- psToken is internal, never decoded from API JSON

-- | The body of @POST /Sessions/Capabilities[/Full]@: every field
-- optional (an empty body @{}@ means all defaults).
data SessionCapabilities = SessionCapabilities
  { scPlayableMediaTypes           :: ![Text]
  , scSupportedCommands            :: ![Text]
  , scSupportsMediaControl         :: !Bool
  , scSupportsPersistentIdentifier :: !Bool
  , scDeviceId                     :: !(Maybe Text)
  } deriving (Eq, Show)

-- | All-default capabilities (empty body case).
emptyCapabilities :: SessionCapabilities
emptyCapabilities = SessionCapabilities [] [] False False Nothing

instance FromJSON SessionCapabilities where
  parseJSON = withObject "SessionCapabilities" $ \o ->
    SessionCapabilities
      <$> o .:? "PlayableMediaTypes" .!= []
      <*> o .:? "SupportedCommands" .!= []
      <*> o .:? "SupportsMediaControl" .!= False
      <*> o .:? "SupportsPersistentIdentifier" .!= False
      <*> o .:? "DeviceId"

-- | Pure: merge reported capabilities onto a session (the lists and
-- flags are replaced, not appended).
applyCapabilities :: PlaySession -> SessionCapabilities -> PlaySession
applyCapabilities s c = s
  { psPlayableMediaTypes           = scPlayableMediaTypes c
  , psSupportedCommands            = scSupportedCommands c
  , psSupportsMediaControl         = scSupportsMediaControl c
  , psSupportsPersistentIdentifier = scSupportsPersistentIdentifier c
  , psDeviceId                     = scDeviceId c
  }

-- | Pure: is this a valid GeneralCommand name? Non-empty, at most 64
-- characters, no control characters (ASCII 0-31 and DEL rejected).
validCommandName :: Text -> Bool
validCommandName n =
  not (T.null n) && T.length n <= 64 && T.all okChar n
  where
    okChar c = let o = ord c in o >= 32 && o /= 127

-- | Pure: the most recently pinged session created with this auth
-- token (Nothing when no session carries the token).
latestSessionForToken :: Text -> [PlaySession] -> Maybe PlaySession
latestSessionForToken tok ss =
  listToMaybe (reverse (sortOn (\s -> (psLastPingAt s, psId s))
                          (filter (\s -> psToken s == Just tok) ss)))
