module FMS.Modules.Metadata.Types
  ( MediaMetadata(..)
  , emptyMetadata
  , metadataStoredJSON
  , editableFieldNames
  , fieldNameFor
  , applyManualPatch
  , applyProviderMerge
  , mergeTags
  , removeTag
  , MetadataSettings (..)
  , MetadataSettingsPatch (..)
  , emptyMetadataSettingsPatch
  , metadataSettingsFromConfig
  , mergeMetadataSettings
  , metadataSettingsPublicJson
  ) where

import Data.Aeson (FromJSON (..), Result (..), ToJSON (..), Value (..), fromJSON, object, withObject, (.!=), (.:?), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.KeyMap (KeyMap)
import Data.Aeson.Types (Parser)
import Data.Char (toLower)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T

import FMS.Core.Types.Config (Config (..))

-- | Metadata for one library item (docs/roadmap.org milestone 4,
-- docs/todo.org section 3). All fields are optional because a source (NFO,
-- later TMDB/TvMaze) may only know some of them. 'metaProviderIds' maps a
-- provider name ("tmdb", "imdb", ...) to the id the item has there;
-- 'metaSource' records where the metadata came from ("nfo", later "tmdb").
-- 'metaLockedFields' (milestone 4k) lists canonical field names that a
-- provider refresh must not overwrite; absent in pre-4k metadata.json.
data MediaMetadata = MediaMetadata
  { metaTitle          :: Maybe Text          -- ^ <title>
  , metaOverview       :: Maybe Text          -- ^ <overview> / plot
  , metaGenres         :: [Text]              -- ^ all <genre> elements
  , metaKeywords       :: [Text]              -- ^ TMDB keywords (m53a; server-side ONLY, never in the public JSON)
  , metaStudios        :: [Text]              -- ^ all <studio> elements (milestone 4h)
  , metaPersons        :: [Text]              -- ^ actor/director/person names (milestone 4h)
  , metaTags           :: [Text]              -- ^ all <tag> elements (milestone 4h)
  , metaYear           :: Maybe Int           -- ^ <year>
  , metaRating         :: Maybe Double        -- ^ <rating>
  , metaOfficialRating :: Maybe Text          -- ^ <mpaa> (milestone 9); absent in old metadata.json
  , metaRuntimeMinutes :: Maybe Int           -- ^ <runtime> in minutes
  , metaProviderIds    :: HashMap Text Text   -- ^ <tmdbid> -> "tmdb", <imdbid> -> "imdb"
  , metaSource         :: Text                -- ^ provenance, e.g. "nfo"
  , metaLockedFields   :: [Text]              -- ^ fields locked against provider refresh (milestone 4k)
  } deriving (Show, Eq)

-- | The empty record: every optional field 'Nothing', every list empty.
-- Used as the base when a manual edit (milestone 4k) targets an item that
-- has no stored metadata yet.
emptyMetadata :: MediaMetadata
emptyMetadata = MediaMetadata
  { metaTitle          = Nothing
  , metaOverview       = Nothing
  , metaGenres         = []
  , metaKeywords       = []
  , metaStudios        = []
  , metaPersons        = []
  , metaTags           = []
  , metaYear           = Nothing
  , metaRating         = Nothing
  , metaOfficialRating = Nothing
  , metaRuntimeMinutes = Nothing
  , metaProviderIds    = HM.empty
  , metaSource         = "unknown"
  , metaLockedFields   = []
  }

instance ToJSON MediaMetadata where
  toJSON m = object
    [ "title"          .= metaTitle m
    , "overview"       .= metaOverview m
    , "genres"         .= metaGenres m
    , "studios"        .= metaStudios m
    , "persons"        .= metaPersons m
    , "tags"           .= metaTags m
    , "year"           .= metaYear m
    , "rating"         .= metaRating m
    , "officialRating" .= metaOfficialRating m
    , "runtimeMinutes" .= metaRuntimeMinutes m
    , "providerIds"    .= metaProviderIds m
    , "source"         .= metaSource m
    , "lockedFields"   .= metaLockedFields m
    ]

-- | m53a: persistence-only JSON for a metadata record — the public
-- 'ToJSON' shape PLUS @keywords@ (server-side only; the public item /
-- metadata API responses never include keywords).
metadataStoredJSON :: MediaMetadata -> Value
metadataStoredJSON m = case toJSON m of
  Object o -> Object (KM.insert (Key.fromText "keywords") (toJSON (metaKeywords m)) o)
  v        -> v

instance FromJSON MediaMetadata where
  parseJSON = withObject "MediaMetadata" $ \o ->
    MediaMetadata
      <$> o .:? "title"
      <*> o .:? "overview"
      <*> o .:? "genres" .!= []
      <*> o .:? "keywords" .!= []    -- m53a; persisted, but NOT in ToJSON (server-side only)
      <*> o .:? "studios" .!= []      -- milestone 4h; absent in pre-4h metadata.json
      <*> o .:? "persons" .!= []
      <*> o .:? "tags" .!= []
      <*> o .:? "year"
      <*> o .:? "rating"
      <*> o .:? "officialRating"      -- milestone 9; absent in pre-9 metadata.json
      <*> o .:? "runtimeMinutes"
      <*> o .:? "providerIds" .!= HM.empty
      <*> o .:? "source" .!= "unknown"
      <*> o .:? "lockedFields" .!= []  -- milestone 4k; absent in pre-4k metadata.json

-- Milestone 4k (manual metadata editing, docs/todo.org section 3) ---------

-- | Canonical field names a manual edit may set; these are exactly the
-- lowercase ToJSON keys of the editable fields (providerIds/source are
-- provenance and not editable, lockedFields is handled separately).
editableFieldNames :: [Text]
editableFieldNames =
  [ "title", "overview", "genres", "studios", "persons", "tags"
  , "year", "rating", "officialRating", "runtimeMinutes"
  ]

-- | Map a JSON object key from a manual-edit body onto the canonical field
-- name, case-insensitively (Jellyfin clients send mixed-case keys).
-- 'Nothing' for unknown / non-editable keys (ignored by 'applyManualPatch').
fieldNameFor :: Text -> Maybe Text
fieldNameFor key =
  let k = T.toLower key
  in case [f | f <- editableFieldNames, T.toLower f == k] of
       (f : _) -> Just f   -- canonical (ToJSON) casing, e.g. "officialRating"
       []      -> Nothing

-- | Decode a 'Value' as @a@, falling back to @def@ when it does not parse
-- (a malformed patch value keeps the old field value instead of failing).
valueOr :: FromJSON a => a -> Value -> a
valueOr def v = case fromJSON v of
  Success a -> a
  Error _   -> def

-- | Set one canonical field of a record from a JSON 'Value'. Unknown field
-- names are the identity. A JSON @null@ clears 'Maybe' fields (aeson decodes
-- null to 'Nothing').
setFieldFromValue :: MediaMetadata -> Text -> Value -> MediaMetadata
setFieldFromValue m f v = case f of
  "title"          -> m { metaTitle          = valueOr (metaTitle m) v }
  "overview"       -> m { metaOverview       = valueOr (metaOverview m) v }
  "genres"         -> m { metaGenres         = valueOr (metaGenres m) v }
  "studios"        -> m { metaStudios        = valueOr (metaStudios m) v }
  "persons"        -> m { metaPersons        = valueOr (metaPersons m) v }
  "tags"           -> m { metaTags           = valueOr (metaTags m) v }
  "year"           -> m { metaYear           = valueOr (metaYear m) v }
  "rating"         -> m { metaRating         = valueOr (metaRating m) v }
  "officialRating" -> m { metaOfficialRating = valueOr (metaOfficialRating m) v }
  "runtimeMinutes" -> m { metaRuntimeMinutes = valueOr (metaRuntimeMinutes m) v }
  _                -> m

-- | The current value of a canonical field as a JSON 'Value' (via ToJSON),
-- for the merge helper. 'Nothing' when @f@ is not a ToJSON key of the record.
getFieldValue :: MediaMetadata -> Text -> Maybe Value
getFieldValue m f = case toJSON m of
  Object o -> KM.lookup (Key.fromText f) o
  _        -> Nothing

-- | Pure (milestone 4k): apply a manual-edit patch (a JSON object as a
-- 'KeyMap') to a stored record. Known fields are set; fields whose
-- canonical name is listed in 'metaLockedFields' (case-insensitive) are
-- skipped; unknown keys are ignored; the @lockedFields@ key itself is
-- always settable (that is how locks are added/removed).
applyManualPatch :: MediaMetadata -> KeyMap Value -> MediaMetadata
applyManualPatch m patch = foldl applyOne m (KM.toList patch)
  where
    locked = map T.toLower (metaLockedFields m)
    applyOne acc (k, v)
      | Key.toText k == "lockedFields" =
          acc { metaLockedFields = valueOr (metaLockedFields acc) v }
      | otherwise =
          case fieldNameFor (Key.toText k) of
            Just f | T.toLower f `notElem` locked -> setFieldFromValue acc f v
            _                                     -> acc

-- | Pure (item tags API): append the @newTags@ that are not already
-- present (case-insensitive comparison) after the existing ones, keeping
-- the existing order. Duplicates inside either list are kept as-is
-- apart from the case-insensitive check.
mergeTags :: [Text] -> [Text] -> [Text]
mergeTags existing newTags = foldl add existing newTags
  where
    add acc t
      | any (\e -> T.toLower e == T.toLower t) acc = acc
      | otherwise = acc ++ [t]

-- | Pure (item tags API): remove every tag equal to @t@
-- (case-insensitive) from the list.
removeTag :: Text -> [Text] -> [Text]
removeTag t = filter (\e -> T.toLower e /= T.toLower t)

-- | Pure (milestone 4k): merge a freshly fetched provider record (@new@)
-- over the stored one (@old@): every field listed in the OLD record's
-- 'metaLockedFields' keeps the old value, all other fields take the new
-- value, and 'metaLockedFields' itself is carried over from the old record.
-- Tags are the exception: 'metaTags' is always the union of old and new
-- ('mergeTags'), so user-added tags survive every provider refresh.
applyProviderMerge :: MediaMetadata -> MediaMetadata -> MediaMetadata
applyProviderMerge old new =
  (foldl keepLocked new { metaLockedFields = metaLockedFields old } locked)
    { metaTags = mergeTags (metaTags old) (metaTags new) }
    { metaKeywords = if null (metaKeywords new) then metaKeywords old else metaKeywords new }  -- m53a
  where
    locked = map T.toLower (metaLockedFields old)
    keepLocked acc f = case getFieldValue old f of
      Just v  -> setFieldFromValue acc f v
      Nothing -> acc

-- m35: runtime TMDB metadata settings -----------------------------------------

-- | Persisted TMDB metadata settings (m35): @stateDir/metadata-settings.json@.
-- On first boot (file absent) seeded from the env config
-- ('metadataSettingsFromConfig'); afterwards the saved values win.
data MetadataSettings = MetadataSettings
  { msTmdbApiKey  :: !Text   -- ^ TMDB API key (secret; redacted for non-admins)
  , msFetchImages :: !Bool   -- ^ download artwork during metadata refresh
  } deriving (Eq, Show)

-- | Persistence JSON: lowercase keys (like metadata.json).
instance ToJSON MetadataSettings where
  toJSON s = object
    [ "tmdbApiKey" .= msTmdbApiKey s
    , "fetchImages" .= msFetchImages s
    ]

instance FromJSON MetadataSettings where
  parseJSON = withObject "MetadataSettings" $ \o ->
    MetadataSettings
      <$> o .:? "tmdbApiKey"  .!= ""
      <*> o .:? "fetchImages" .!= False

-- | A partial update posted to @POST /Metadata/Settings@ (m35).
data MetadataSettingsPatch = MetadataSettingsPatch
  { mspTmdbApiKey  :: Maybe Text
  , mspFetchImages :: Maybe Bool
  }

emptyMetadataSettingsPatch :: MetadataSettingsPatch
emptyMetadataSettingsPatch = MetadataSettingsPatch Nothing Nothing

-- | Decode a possibly-present field under its PascalCase OR camelCase key.
fieldAnyMeta :: FromJSON a => Text -> KeyMap Value -> Parser (Maybe a)
fieldAnyMeta name o =
  case KM.lookup (Key.fromText name) o of
    Just v  -> Just <$> parseJSON v
    Nothing -> case KM.lookup (Key.fromText (lowerFirst name)) o of
      Just v  -> Just <$> parseJSON v
      Nothing -> pure Nothing
  where
    lowerFirst t = case T.uncons t of
      Just (c, rest) -> T.cons (toLower c) rest
      Nothing        -> t

-- | The settings POST body: any subset of {TmdbApiKey, FetchImages},
-- PascalCase or camelCase keys.
instance FromJSON MetadataSettingsPatch where
  parseJSON = withObject "MetadataSettingsPatch" $ \o ->
    MetadataSettingsPatch
      <$> fieldAnyMeta "TmdbApiKey" o
      <*> fieldAnyMeta "FetchImages" o

-- | Pure: seed the settings from the env-derived config (first boot only).
metadataSettingsFromConfig :: Config -> MetadataSettings
metadataSettingsFromConfig cfg = MetadataSettings
  { msTmdbApiKey = configTmdbApiKey cfg
  , msFetchImages = configFetchImages cfg
  }

-- | Pure: apply a partial update (merge).
mergeMetadataSettings :: MetadataSettings -> MetadataSettingsPatch -> MetadataSettings
mergeMetadataSettings s p = s
  { msTmdbApiKey = maybe (msTmdbApiKey s) id (mspTmdbApiKey p)
  , msFetchImages = maybe (msFetchImages s) id (mspFetchImages p)
  }

-- | API JSON (PascalCase). SECURITY: 'TmdbApiKey' is redacted to null for
-- non-admins (same pattern as DelugePassword / indexer ApiKey).
metadataSettingsPublicJson :: Bool -> MetadataSettings -> Value
metadataSettingsPublicJson isAdmin s = object
  [ "TmdbApiKey" .= (if isAdmin then Aeson.String (msTmdbApiKey s) else Aeson.Null)
  , "FetchImages" .= msFetchImages s
  ]
