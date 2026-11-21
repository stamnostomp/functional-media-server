module FMS.Modules.Requests.Types
  ( Request (..)
  , RequestStatus (..)
  , RequestSettings (..)
  , SettingsPatch (..)
  , emptySettingsPatch
  , settingsPublicJson
  , mergeSettings
  , settingsFromConfig
  , effectiveAutoApproveGb
  , normalizeTitle
  , isActiveStatus
  , duplicateOf
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, (.:), (.:?), (.!=), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson.Types (Parser)
import Data.Char (isAlphaNum, toLower)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)

import FMS.Core.Types.Config (Config (..))

-- | Lifecycle of a media request (m20a; Radarr/Sonarr-style queue).
-- JSON string values are the constructor names: "Pending", "Approved",
-- "Downloading", "Imported", "Rejected", "Failed".
data RequestStatus
  = Pending
  | Approved
  | Downloading
  | Imported
  | Rejected
  | Failed
  deriving (Eq, Show)

instance ToJSON RequestStatus where
  toJSON s = toJSON (statusName s)

instance FromJSON RequestStatus where
  parseJSON v = do
    str <- parseJSON v
    case str of
      "Pending"     -> pure Pending
      "Approved"    -> pure Approved
      "Downloading" -> pure Downloading
      "Imported"    -> pure Imported
      "Rejected"    -> pure Rejected
      "Failed"      -> pure Failed
      other         -> fail ("unknown RequestStatus: " ++ T.unpack other)

statusName :: RequestStatus -> Text
statusName s = case s of
  Pending     -> "Pending"
  Approved    -> "Approved"
  Downloading -> "Downloading"
  Imported    -> "Imported"
  Rejected    -> "Rejected"
  Failed      -> "Failed"

-- | One media request. The API JSON is PascalCase (the convention);
-- persistence uses lowercase keys (see Persistence).
data Request = Request
  { reqId           :: !Text            -- ^ stable id (16 hex chars)
  , reqUserId       :: !Text
  , reqUserName     :: !Text
  , reqTitle        :: !Text
  , reqYear         :: !(Maybe Int)
  , reqTmdbId       :: !(Maybe Int)     -- ^ m68: TMDB id from the Discover card, so the content is known WITHOUT name-sanitizing; metadata is resolved from this id first, name-search is the fallback
  , reqSeason       :: !(Maybe Int)     -- ^ m76: season of a tv episode/season request (stored BEFORE download; used to write the .nfo at import)
  , reqEpisode      :: !(Maybe Int)     -- ^ m76: episode number (Nothing for a whole-season request)
  , reqMediaType    :: !Text            -- ^ "movie" or "tv"
  , reqQuery        :: !Text
  , reqStatus       :: !RequestStatus
  , reqSizeBytes    :: !(Maybe Integer)
  , reqSourceIndexer :: !(Maybe Text)
  , reqSourceRelease :: !(Maybe Text)
  , reqSourceUrl    :: !(Maybe Text)   -- ^ release link (torrent/magnet); stored, not secret
  , reqMagnet       :: !(Maybe Text)   -- ^ m20b: magnet URI chosen for download
  , reqDownloadUrl  :: !(Maybe Text)   -- ^ m20b: .torrent URL chosen for download
  , reqCreatedAt    :: !UTCTime
  , reqUpdatedAt    :: !UTCTime
  , reqApprovedBy   :: !(Maybe Text)
  , reqError        :: !(Maybe Text)
  , reqDownloadDir  :: !(Maybe Text)   -- ^ m27: download location stamped at creation from the effective settings
  , reqProgress     :: !(Maybe Double) -- ^ m37: live Deluge progress 0-100 (not persisted; filled on read)
  , reqTorrentState :: !(Maybe Text)   -- ^ m37: live Deluge torrent state (not persisted; filled on read)
  } deriving (Eq, Show)

-- | API JSON (PascalCase keys, the API convention).
instance ToJSON Request where
  toJSON r = object
    [ "Id"           .= reqId r
    , "UserId"       .= reqUserId r
    , "UserName"     .= reqUserName r
    , "Title"        .= reqTitle r
    , "Year"         .= reqYear r
    , "TmdbId"       .= reqTmdbId r
    , "Season"       .= reqSeason r   -- m76
    , "Episode"      .= reqEpisode r  -- m76
    , "MediaType"    .= reqMediaType r
    , "Query"        .= reqQuery r
    , "Status"       .= reqStatus r
    , "SizeBytes"    .= reqSizeBytes r
    , "SourceIndexer" .= reqSourceIndexer r
    , "SourceRelease" .= reqSourceRelease r
    , "SourceUrl"    .= reqSourceUrl r
    , "Magnet"       .= reqMagnet r
    , "DownloadUrl"  .= reqDownloadUrl r
    , "CreatedAt"    .= reqCreatedAt r
    , "UpdatedAt"    .= reqUpdatedAt r
    , "ApprovedBy"   .= reqApprovedBy r
    , "Error"        .= reqError r
    , "DownloadDir"  .= reqDownloadDir r
    , "Progress"     .= reqProgress r      -- m37: live-only
    , "TorrentState" .= reqTorrentState r  -- m37: live-only
    ]

instance FromJSON Request where
  parseJSON = withObject "Request" $ \o ->
    Request
      <$> o .:  "Id"
      <*> o .:  "UserId"
      <*> o .:  "UserName"
      <*> o .:  "Title"
      <*> o .:? "Year"
      <*> o .:? "TmdbId"
      <*> o .:? "Season"       -- m76
      <*> o .:? "Episode"      -- m76
      <*> o .:? "MediaType"    .!= "movie"
      <*> o .:? "Query"        .!= ""
      <*> o .:  "Status"
      <*> o .:? "SizeBytes"
      <*> o .:? "SourceIndexer"
      <*> o .:? "SourceRelease"
      <*> o .:? "SourceUrl"
      <*> o .:? "Magnet"
      <*> o .:? "DownloadUrl"
      <*> o .:  "CreatedAt"
      <*> o .:  "UpdatedAt"
      <*> o .:? "ApprovedBy"
      <*> o .:? "Error"
      <*> o .:? "DownloadDir"
      <*> o .:? "Progress"      -- m37: live-only, absent in API payloads sent by clients
      <*> o .:? "TorrentState"

-- | Pure: normalize a title for duplicate detection -- lowercase, keep
-- alphanumerics only (everything else becomes a space), collapse and
-- trim whitespace. "The Matrix (1999)" and "the matrix 1999" both
-- normalize to "the matrix 1999".
normalizeTitle :: Text -> Text
normalizeTitle =
  T.unwords . T.words . T.map (\c -> if isAlphaNum c then toLower c else ' ')

-- | Pure: is this status still in flight (cancellable / not yet resolved)?
isActiveStatus :: RequestStatus -> Bool
isActiveStatus s = case s of
  Pending     -> True
  Approved    -> True
  Downloading -> True
  Imported    -> False
  Rejected    -> False
  Failed      -> False

-- | Pure: the active request with the same normalized title, if any
-- (duplicate detection across ALL users, m20a).
duplicateOf :: Text -> [Request] -> Maybe Request
duplicateOf title reqs =
  find (\r -> isActiveStatus (reqStatus r)
              && normalizeTitle (reqTitle r) == normalizeTitle title) reqs

-- m27: runtime-configurable request/download-client settings -----------------

-- | Persisted download-client settings (m27): @stateDir/requests-settings.json@.
-- On first boot (file absent) the values are seeded from the env config
-- ('settingsFromConfig'); afterwards the saved values win.
data RequestSettings = RequestSettings
  { rsDelugeUrl       :: !Text          -- ^ Deluge web JSON-RPC endpoint
  , rsDelugePassword  :: !Text          -- ^ secret; redacted for non-admins
  , rsDownloadDir     :: !Text          -- ^ download dir pushed to Deluge + auto-import folder
  , rsAutoApproveGb   :: !(Maybe Double) -- ^ auto-approve threshold in GiB (Nothing = use env/default)
  } deriving (Eq, Show)

-- | Persistence JSON: lowercase keys (like requests.json).
instance ToJSON RequestSettings where
  toJSON s = object
    [ "delugeUrl" .= rsDelugeUrl s
    , "delugePassword" .= rsDelugePassword s
    , "downloadDir" .= rsDownloadDir s
    , "autoApproveGb" .= rsAutoApproveGb s
    ]

instance FromJSON RequestSettings where
  parseJSON = withObject "RequestSettings" $ \o ->
    RequestSettings
      <$> o .:? "delugeUrl"      .!= ""
      <*> o .:? "delugePassword" .!= ""
      <*> o .:? "downloadDir"    .!= ""
      <*> o .:? "autoApproveGb"

-- | A partial update posted to @POST /Requests/Settings@. Outer 'Nothing'
-- = leave unchanged; for 'spAutoApproveGb' the inner 'Nothing' clears the
-- override (back to the env/default threshold).
data SettingsPatch = SettingsPatch
  { spDelugeUrl      :: Maybe Text
  , spDelugePassword :: Maybe Text
  , spDownloadDir    :: Maybe Text
  , spAutoApproveGb  :: Maybe (Maybe Double)
  }

emptySettingsPatch :: SettingsPatch
emptySettingsPatch = SettingsPatch Nothing Nothing Nothing Nothing

-- | Decode a possibly-present field under its PascalCase OR camelCase key
-- (m27 settings POST accepts both shapes).
fieldAny :: FromJSON a => Text -> KeyMap.KeyMap Value -> Parser (Maybe a)
fieldAny name o =
  case KeyMap.lookup (Key.fromText name) o of
    Just v  -> Just <$> parseJSON v
    Nothing -> case KeyMap.lookup (Key.fromText (lowerFirst name)) o of
      Just v  -> Just <$> parseJSON v
      Nothing -> pure Nothing
  where
    lowerFirst t = case T.uncons t of
      Just (c, rest) -> T.cons (toLower c) rest
      Nothing        -> t

-- | The settings POST body: any subset of the fields, PascalCase or
-- camelCase keys. @AutoApproveGb@ may be a number or null (clears the
-- override, back to env/default).
instance FromJSON SettingsPatch where
  parseJSON = withObject "SettingsPatch" $ \o ->
    SettingsPatch
      <$> fieldAny "DelugeUrl" o
      <*> fieldAny "DelugePassword" o
      <*> fieldAny "DownloadDir" o
      <*> fieldAny "AutoApproveGb" o

-- | Pure: seed the settings from the env-derived config (first boot only).
settingsFromConfig :: Config -> RequestSettings
settingsFromConfig cfg = RequestSettings
  { rsDelugeUrl = configDelugeUrl cfg
  , rsDelugePassword = configDelugePassword cfg
  , rsDownloadDir = T.pack (configDownloadDir cfg)
  , rsAutoApproveGb = Just (fromIntegral (configRequestAutoApproveGb cfg))
  }

-- | Pure: apply a partial update (merge).
mergeSettings :: RequestSettings -> SettingsPatch -> RequestSettings
mergeSettings s p = s
  { rsDelugeUrl = maybe (rsDelugeUrl s) id (spDelugeUrl p)
  , rsDelugePassword = maybe (rsDelugePassword s) id (spDelugePassword p)
  , rsDownloadDir = maybe (rsDownloadDir s) id (spDownloadDir p)
  , rsAutoApproveGb = case spAutoApproveGb p of
      Nothing -> rsAutoApproveGb s
      Just m  -> m
  }

-- | Pure: effective auto-approve threshold in GiB -- the saved override
-- wins, else the env/default from the config.
effectiveAutoApproveGb :: Config -> RequestSettings -> Double
effectiveAutoApproveGb cfg s =
  maybe (fromIntegral (configRequestAutoApproveGb cfg)) id (rsAutoApproveGb s)

-- | API JSON (PascalCase). SECURITY: 'DelugePassword' is redacted to null
-- for non-admins (same pattern as indexer ApiKey/Cookie).
settingsPublicJson :: Bool -> RequestSettings -> Value
settingsPublicJson isAdmin s = object
  [ "DelugeUrl" .= rsDelugeUrl s
  , "DelugePassword" .= (if isAdmin then Aeson.String (rsDelugePassword s) else Aeson.Null)
  , "DownloadDir" .= rsDownloadDir s
  , "AutoApproveGb" .= rsAutoApproveGb s
  ]
