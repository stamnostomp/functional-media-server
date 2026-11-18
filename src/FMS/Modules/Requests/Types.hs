module FMS.Modules.Requests.Types
  ( Request (..)
  , RequestStatus (..)
  , normalizeTitle
  , isActiveStatus
  , duplicateOf
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.!=), (.=))
import Data.Char (isAlphaNum, toLower)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)

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
  , reqMediaType    :: !Text            -- ^ "movie" or "tv"
  , reqQuery        :: !Text
  , reqStatus       :: !RequestStatus
  , reqSizeBytes    :: !(Maybe Integer)
  , reqSourceIndexer :: !(Maybe Text)
  , reqSourceRelease :: !(Maybe Text)
  , reqSourceUrl    :: !(Maybe Text)   -- ^ release link (torrent/magnet); stored, not secret
  , reqCreatedAt    :: !UTCTime
  , reqUpdatedAt    :: !UTCTime
  , reqApprovedBy   :: !(Maybe Text)
  , reqError        :: !(Maybe Text)
  } deriving (Eq, Show)

-- | API JSON (PascalCase keys, the API convention).
instance ToJSON Request where
  toJSON r = object
    [ "Id"           .= reqId r
    , "UserId"       .= reqUserId r
    , "UserName"     .= reqUserName r
    , "Title"        .= reqTitle r
    , "Year"         .= reqYear r
    , "MediaType"    .= reqMediaType r
    , "Query"        .= reqQuery r
    , "Status"       .= reqStatus r
    , "SizeBytes"    .= reqSizeBytes r
    , "SourceIndexer" .= reqSourceIndexer r
    , "SourceRelease" .= reqSourceRelease r
    , "SourceUrl"    .= reqSourceUrl r
    , "CreatedAt"    .= reqCreatedAt r
    , "UpdatedAt"    .= reqUpdatedAt r
    , "ApprovedBy"   .= reqApprovedBy r
    , "Error"        .= reqError r
    ]

instance FromJSON Request where
  parseJSON = withObject "Request" $ \o ->
    Request
      <$> o .:  "Id"
      <*> o .:  "UserId"
      <*> o .:  "UserName"
      <*> o .:  "Title"
      <*> o .:? "Year"
      <*> o .:? "MediaType"    .!= "movie"
      <*> o .:? "Query"        .!= ""
      <*> o .:  "Status"
      <*> o .:? "SizeBytes"
      <*> o .:? "SourceIndexer"
      <*> o .:? "SourceRelease"
      <*> o .:? "SourceUrl"
      <*> o .:  "CreatedAt"
      <*> o .:  "UpdatedAt"
      <*> o .:? "ApprovedBy"
      <*> o .:? "Error"

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
