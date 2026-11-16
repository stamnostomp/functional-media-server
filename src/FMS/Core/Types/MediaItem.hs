{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module FMS.Core.Types.MediaItem
  ( MediaItem(..)
  , MediaItemV0(..)
  , MediaItemV1(..)
  , migrateMediaItem
  , migrateMediaItemV1
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
-- * v1 (milestone 2c): + 'itemFolder' (default "1" on migrate).
-- * v2 (milestone 4d): + 'itemType' / 'itemSeries' / 'itemSeason' /
--   'itemEpisode' (defaults on migrate: type from the mime, rest Nothing).
data MediaItem = MediaItem
  { itemId      :: !Text      -- ^ stable id, assigned by 'RegisterItem'
  , itemTitle   :: !Text      -- ^ base name of the file, without extension
  , itemPath    :: !FilePath  -- ^ path on disk
  , itemSize    :: !Integer   -- ^ size in bytes
  , itemMime    :: !Text      -- ^ mime type from the extension map
  , itemAddedAt :: !UTCTime   -- ^ file modification time
  , itemFolder  :: !Text      -- ^ id of the media folder this file was scanned from (milestone 2c)
  , itemType    :: !Text      -- ^ \"Movie\" | \"Episode\" | \"Audio\" | \"Unknown\" (milestone 4d)
  , itemSeries  :: !(Maybe Text)  -- ^ series name for episodes (milestone 4d)
  , itemSeason  :: !(Maybe Int)   -- ^ season number for episodes (milestone 4d)
  , itemEpisode :: !(Maybe Int)   -- ^ episode number for episodes (milestone 4d)
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
-- v0 -> v1 -> v2.
data MediaItemV1 = MediaItemV1
  { v1ItemId      :: !Text
  , v1ItemTitle   :: !Text
  , v1ItemPath    :: !FilePath
  , v1ItemSize    :: !Integer
  , v1ItemMime    :: !Text
  , v1ItemAddedAt :: !UTCTime
  , v1ItemFolder  :: !Text
  } deriving (Show, Eq)

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

-- | Current shape: version 2, extending v1.
deriveSafeCopy 2 'extension ''MediaItem

-- | Migrate a v1 item to v2 (milestone 4d): the new fields default from
-- what is already known -- 'itemType' from the mime, series/season/episode
-- to 'Nothing'.
instance Migrate MediaItem where
  type MigrateFrom MediaItem = MediaItemV1
  migrate v1 = MediaItem
    { itemId      = v1ItemId v1
    , itemTitle   = v1ItemTitle v1
    , itemPath    = v1ItemPath v1
    , itemSize    = v1ItemSize v1
    , itemMime    = v1ItemMime v1
    , itemAddedAt = v1ItemAddedAt v1
    , itemFolder  = v1ItemFolder v1
    , itemType    = itemTypeFromMime (v1ItemMime v1)
    , itemSeries  = Nothing
    , itemSeason  = Nothing
    , itemEpisode = Nothing
    }

-- | Pure: the item type implied by a mime type (used by the v1->v2
-- migration and by the scanner for non-episode files).
itemTypeFromMime :: Text -> Text
itemTypeFromMime mime
  | "video" `T.isPrefixOf` mime = "Movie"
  | "audio" `T.isPrefixOf` mime = "Audio"
  | otherwise                   = "Unknown"

-- | Descriptive aliases for the 'Migrate' methods (for tests/readability).
migrateMediaItem :: MediaItemV0 -> MediaItemV1
migrateMediaItem = migrate

migrateMediaItemV1 :: MediaItemV1 -> MediaItem
migrateMediaItemV1 = migrate

instance ToJSON MediaItem where
  toJSON i = object
    [ "id"       .= itemId i
    , "title"    .= itemTitle i
    , "path"     .= itemPath i
    , "size"     .= itemSize i
    , "mime"     .= itemMime i
    , "addedAt"  .= itemAddedAt i
    , "folderId" .= itemFolder i
    , "type"     .= itemType i
    , "series"   .= itemSeries i
    , "season"   .= itemSeason i
    , "episode"  .= itemEpisode i
    ]
