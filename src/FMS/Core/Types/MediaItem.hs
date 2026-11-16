{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module FMS.Core.Types.MediaItem
  ( MediaItem(..)
  , MediaItemV0(..)
  , MediaItemV1(..)
  , MediaItemV2(..)
  , migrateMediaItem
  , migrateMediaItemV1
  , migrateMediaItemV2
  , itemTypeFromMime
  ) where

import Data.Aeson (ToJSON(..), object, (.=))
import Data.SafeCopy (Migrate (..), base, deriveSafeCopy, extension)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)

-- | One library item (docs/modules.org). The 'itemId' is a stable id
-- assigned by acid-state (see "FMS.Modules.Library.State"), NOT a hash of
-- the file path — that was the v1 limitation fixed in milestone 2
-- (docs/acid-state.org).
--
-- SafeCopy history (docs/acid-state.org):
-- * v0: original fields; kept as 'MediaItemV0' so old acid-state dirs load.
-- * v1 (milestone 2c): + 'itemFolder' (default \"1\" on migrate).
-- * v2 (milestone 4d): + 'itemType' / 'itemSeries' / 'itemSeason' /
--   'itemEpisode' (defaults on migrate: type from the mime, rest Nothing).
-- * v3 (milestone 4g): + 'itemExtraType' (Extras; default Nothing).
data MediaItem = MediaItem
  { itemId         :: !Text      -- ^ stable id, assigned by 'RegisterItem'
  , itemTitle      :: !Text      -- ^ base name of the file, without extension
  , itemPath       :: !FilePath  -- ^ path on disk
  , itemSize       :: !Integer   -- ^ size in bytes
  , itemMime       :: !Text      -- ^ mime type from the extension map
  , itemAddedAt    :: !UTCTime   -- ^ file modification time
  , itemFolder     :: !Text      -- ^ id of the media folder this file was scanned from (milestone 2c)
  , itemType       :: !Text      -- ^ \"Movie\" | \"Episode\" | \"Audio\" | \"Extra\" | \"Unknown\" (milestone 4d/4g)
  , itemSeries     :: !(Maybe Text)  -- ^ series name for episodes (milestone 4d)
  , itemSeason     :: !(Maybe Int)   -- ^ season number for episodes (milestone 4d)
  , itemEpisode    :: !(Maybe Int)   -- ^ episode number for episodes (milestone 4d)
  , itemExtraType  :: !(Maybe Text)  -- ^ \"Trailer\" | \"BehindTheScenes\" | ... for Extra items (milestone 4g)
  } deriving (Show, Eq)

-- | The pre-2c shape (no 'itemFolder'), kept for safecopy v0.
data MediaItemV0 = MediaItemV0
  { v0ItemId      :: !Text
  , v0ItemTitle   :: !Text
  , v0ItemPath    :: !FilePath
  , v0ItemSize    :: !Integer
  , v0ItemMime    :: !Text
  , v0ItemAddedAt :: !UTCTime
  } deriving (Show, Eq)

-- | The milestone-2c shape (with 'itemFolder', without the 4d fields),
-- kept as version 1 so pre-4d acid-state dirs load through the chain
-- v0 -> v1 -> v2 -> v3.
data MediaItemV1 = MediaItemV1
  { v1ItemId      :: !Text
  , v1ItemTitle   :: !Text
  , v1ItemPath    :: !FilePath
  , v1ItemSize    :: !Integer
  , v1ItemMime    :: !Text
  , v1ItemAddedAt :: !UTCTime
  , v1ItemFolder  :: !Text
  } deriving (Show, Eq)

-- | The milestone-4d shape (with the type/series/season/episode fields,
-- without 'itemExtraType'), kept as version 2 so pre-4g acid-state dirs
-- load through the chain v0 -> v1 -> v2 -> v3.
data MediaItemV2 = MediaItemV2
  { v2ItemId      :: !Text
  , v2ItemTitle   :: !Text
  , v2ItemPath    :: !FilePath
  , v2ItemSize    :: !Integer
  , v2ItemMime    :: !Text
  , v2ItemAddedAt :: !UTCTime
  , v2ItemFolder  :: !Text
  , v2ItemType    :: !Text
  , v2ItemSeries  :: !(Maybe Text)
  , v2ItemSeason  :: !(Maybe Int)
  , v2ItemEpisode :: !(Maybe Int)
  } deriving (Show, Eq)

-- | Pure: the item type implied by a mime type (used by the v1->v2
-- migration and by the scanner for non-episode files). Defined before the
-- 'deriveSafeCopy' splices because of GHC's TH staging restriction: a
-- binding is only in scope in its own and later declaration groups.
itemTypeFromMime :: Text -> Text
itemTypeFromMime mime
  | "video" `T.isPrefixOf` mime = "Movie"
  | "audio" `T.isPrefixOf` mime = "Audio"
  | otherwise                   = "Unknown"

deriveSafeCopy 0 'base ''MediaItemV0

deriveSafeCopy 1 'extension ''MediaItemV1

instance Migrate MediaItemV1 where
  type MigrateFrom MediaItemV1 = MediaItemV0
  migrate v0 = MediaItemV1
    { v1ItemId      = v0ItemId v0
    , v1ItemTitle   = v0ItemTitle v0
    , v1ItemPath    = v0ItemPath v0
    , v1ItemSize    = v0ItemSize v0
    , v1ItemMime    = v0ItemMime v0
    , v1ItemAddedAt = v0ItemAddedAt v0
    , v1ItemFolder  = "1"   -- pre-2c items belong to the default folder
    }

deriveSafeCopy 2 'extension ''MediaItemV2

-- | Migrate a v1 item to v2 (milestone 4d): the new fields default from
-- what is already known -- 'v2ItemType' from the mime, series/season/
-- episode to 'Nothing'.
instance Migrate MediaItemV2 where
  type MigrateFrom MediaItemV2 = MediaItemV1
  migrate v1 = MediaItemV2
    { v2ItemId      = v1ItemId v1
    , v2ItemTitle   = v1ItemTitle v1
    , v2ItemPath    = v1ItemPath v1
    , v2ItemSize    = v1ItemSize v1
    , v2ItemMime    = v1ItemMime v1
    , v2ItemAddedAt = v1ItemAddedAt v1
    , v2ItemFolder  = v1ItemFolder v1
    , v2ItemType    = itemTypeFromMime (v1ItemMime v1)
    , v2ItemSeries  = Nothing
    , v2ItemSeason  = Nothing
    , v2ItemEpisode = Nothing
    }

-- | Current shape: version 3, extending v2.
deriveSafeCopy 3 'extension ''MediaItem

-- | Migrate a v2 item to v3 (milestone 4g): 'itemExtraType' defaults to
-- 'Nothing' (pre-4g items are not Extras).
instance Migrate MediaItem where
  type MigrateFrom MediaItem = MediaItemV2
  migrate v2 = MediaItem
    { itemId        = v2ItemId v2
    , itemTitle     = v2ItemTitle v2
    , itemPath      = v2ItemPath v2
    , itemSize      = v2ItemSize v2
    , itemMime      = v2ItemMime v2
    , itemAddedAt   = v2ItemAddedAt v2
    , itemFolder    = v2ItemFolder v2
    , itemType      = v2ItemType v2
    , itemSeries    = v2ItemSeries v2
    , itemSeason    = v2ItemSeason v2
    , itemEpisode   = v2ItemEpisode v2
    , itemExtraType = Nothing
    }

-- | Descriptive aliases for the 'Migrate' methods (for tests/readability).
migrateMediaItem :: MediaItemV0 -> MediaItemV1
migrateMediaItem = migrate

migrateMediaItemV1 :: MediaItemV1 -> MediaItemV2
migrateMediaItemV1 = migrate

migrateMediaItemV2 :: MediaItemV2 -> MediaItem
migrateMediaItemV2 = migrate

instance ToJSON MediaItem where
  toJSON i = object
    [ "id"        .= itemId i
    , "title"     .= itemTitle i
    , "path"      .= itemPath i
    , "size"      .= itemSize i
    , "mime"      .= itemMime i
    , "addedAt"   .= itemAddedAt i
    , "folderId"  .= itemFolder i
    , "type"      .= itemType i
    , "series"    .= itemSeries i
    , "season"    .= itemSeason i
    , "episode"   .= itemEpisode i
    , "extraType" .= itemExtraType i
    ]
