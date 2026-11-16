{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module FMS.Core.Types.MediaItem
  ( MediaItem(..)
  , MediaItemV0(..)
  , MediaItemV1(..)
  , MediaItemV2(..)
  , MediaItemV3(..)
  , migrateMediaItem
  , migrateMediaItemV1
  , migrateMediaItemV2
  , migrateMediaItemV3
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
-- * v4 (milestone 4j): + 'itemVersions' (multi-version media; default []
--   on migrate — 'itemPath' stays the primary/only source).
data MediaItem = MediaItem
  { itemId         :: !Text      -- ^ stable id, assigned by 'RegisterItem'
  , itemTitle      :: !Text      -- ^ base name of the file, without extension
  , itemPath       :: !FilePath  -- ^ primary path on disk (== head of itemVersions when non-empty)
  , itemSize       :: !Integer   -- ^ size of the PRIMARY file in bytes
  , itemMime       :: !Text      -- ^ mime type from the extension map
  , itemAddedAt    :: !UTCTime   -- ^ file modification time of the primary
  , itemFolder     :: !Text      -- ^ id of the media folder this file was scanned from (milestone 2c)
  , itemType       :: !Text      -- ^ \"Movie\" | \"Episode\" | \"Audio\" | \"Extra\" | \"Unknown\" (milestone 4d/4g)
  , itemSeries     :: !(Maybe Text)  -- ^ series name for episodes (milestone 4d)
  , itemSeason     :: !(Maybe Int)   -- ^ season number for episodes (milestone 4d)
  , itemEpisode    :: !(Maybe Int)   -- ^ episode number for episodes (milestone 4d)
  , itemExtraType  :: !(Maybe Text)  -- ^ \"Trailer\" | \"BehindTheScenes\" | ... for Extra items (milestone 4g)
  , itemVersions   :: ![FilePath]    -- ^ ALL version paths, best first (milestone 4j); [] = single-file item
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

-- | The milestone-4g shape (with 'itemExtraType', without 'itemVersions'),
-- kept as version 3 so pre-4j acid-state dirs load through the chain
-- v0 -> v1 -> v2 -> v3 -> v4.
data MediaItemV3 = MediaItemV3
  { v3ItemId        :: !Text
  , v3ItemTitle     :: !Text
  , v3ItemPath      :: !FilePath
  , v3ItemSize      :: !Integer
  , v3ItemMime      :: !Text
  , v3ItemAddedAt   :: !UTCTime
  , v3ItemFolder    :: !Text
  , v3ItemType      :: !Text
  , v3ItemSeries    :: !(Maybe Text)
  , v3ItemSeason    :: !(Maybe Int)
  , v3ItemEpisode   :: !(Maybe Int)
  , v3ItemExtraType :: !(Maybe Text)
  } deriving (Show, Eq)

-- | Version 3 extends v2.
deriveSafeCopy 3 'extension ''MediaItemV3

-- | Migrate a v2 item to v3 (milestone 4g): 'v3ItemExtraType' defaults to
-- 'Nothing' (pre-4g items are not Extras).
instance Migrate MediaItemV3 where
  type MigrateFrom MediaItemV3 = MediaItemV2
  migrate v2 = MediaItemV3
    { v3ItemId        = v2ItemId v2
    , v3ItemTitle     = v2ItemTitle v2
    , v3ItemPath      = v2ItemPath v2
    , v3ItemSize      = v2ItemSize v2
    , v3ItemMime      = v2ItemMime v2
    , v3ItemAddedAt   = v2ItemAddedAt v2
    , v3ItemFolder    = v2ItemFolder v2
    , v3ItemType      = v2ItemType v2
    , v3ItemSeries    = v2ItemSeries v2
    , v3ItemSeason    = v2ItemSeason v2
    , v3ItemEpisode   = v2ItemEpisode v2
    , v3ItemExtraType = Nothing
    }

-- | Current shape: version 4, extending v3.
deriveSafeCopy 4 'extension ''MediaItem

-- | Migrate a v3 item to v4 (milestone 4j): 'itemVersions' defaults to []
-- — pre-4j items are single-file, 'itemPath' is their only source.
instance Migrate MediaItem where
  type MigrateFrom MediaItem = MediaItemV3
  migrate v3 = MediaItem
    { itemId        = v3ItemId v3
    , itemTitle     = v3ItemTitle v3
    , itemPath      = v3ItemPath v3
    , itemSize      = v3ItemSize v3
    , itemMime      = v3ItemMime v3
    , itemAddedAt   = v3ItemAddedAt v3
    , itemFolder    = v3ItemFolder v3
    , itemType      = v3ItemType v3
    , itemSeries    = v3ItemSeries v3
    , itemSeason    = v3ItemSeason v3
    , itemEpisode   = v3ItemEpisode v3
    , itemExtraType = v3ItemExtraType v3
    , itemVersions  = []
    }

-- | Descriptive aliases for the 'Migrate' methods (for tests/readability).
migrateMediaItem :: MediaItemV0 -> MediaItemV1
migrateMediaItem = migrate

migrateMediaItemV1 :: MediaItemV1 -> MediaItemV2
migrateMediaItemV1 = migrate

migrateMediaItemV2 :: MediaItemV2 -> MediaItemV3
migrateMediaItemV2 = migrate

migrateMediaItemV3 :: MediaItemV3 -> MediaItem
migrateMediaItemV3 = migrate

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
    , "versions"  .= itemVersions i   -- milestone 4j
    ]
