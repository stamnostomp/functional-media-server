{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module FMS.Core.Types.MediaItem
  ( MediaItem(..)
  , MediaItemV0(..)
  , migrateMediaItem
  ) where

import Data.Aeson (ToJSON(..), object, (.=))
import Data.SafeCopy (Migrate (..), base, deriveSafeCopy, extension)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

-- | One library item (docs/modules.org). The 'itemId' is a stable id
-- assigned by acid-state (see "FMS.Modules.Library.State"), NOT a hash of
-- the file path — that was the v1 limitation fixed in milestone 2
-- (docs/acid-state.org).
--
-- Milestone 2c (media folders) added 'itemFolder'; the SafeCopy version is
-- bumped to 1 (kind 'extension') with the 'Migrate' instance below mapping
-- v0 items to the default folder "1", so existing acid-state dirs keep
-- loading (docs/acid-state.org).
data MediaItem = MediaItem
  { itemId      :: !Text      -- ^ stable id, assigned by 'RegisterItem'
  , itemTitle   :: !Text      -- ^ base name of the file, without extension
  , itemPath    :: !FilePath  -- ^ path on disk
  , itemSize    :: !Integer   -- ^ size in bytes
  , itemMime    :: !Text      -- ^ mime type from the extension map
  , itemAddedAt :: !UTCTime   -- ^ file modification time
  , itemFolder  :: !Text      -- ^ id of the media folder this file was scanned from (milestone 2c)
  } deriving (Show, Eq)

-- | The pre-2c shape of 'MediaItem' (no 'itemFolder'), kept only so
-- safecopy can deserialize old (version 0) acid-state events/checkpoints.
data MediaItemV0 = MediaItemV0
  { v0ItemId      :: !Text
  , v0ItemTitle   :: !Text
  , v0ItemPath    :: !FilePath
  , v0ItemSize    :: !Integer
  , v0ItemMime    :: !Text
  , v0ItemAddedAt :: !UTCTime
  } deriving (Show, Eq)

deriveSafeCopy 0 'base ''MediaItemV0

-- | SafeCopy for the current shape: version 1, extending v0 via 'Migrate'.
deriveSafeCopy 1 'extension ''MediaItem

-- | Migrate a v0 item to v1: the new 'itemFolder' defaults to "1", the id
-- of the single default media folder (milestone 2c), so pre-2c libraries
-- keep their items tagged with the default folder.
instance Migrate MediaItem where
  type MigrateFrom MediaItem = MediaItemV0
  migrate v0 = MediaItem
    { itemId      = v0ItemId v0
    , itemTitle   = v0ItemTitle v0
    , itemPath    = v0ItemPath v0
    , itemSize    = v0ItemSize v0
    , itemMime    = v0ItemMime v0
    , itemAddedAt = v0ItemAddedAt v0
    , itemFolder  = "1"
    }

-- | Descriptive alias for the 'Migrate' method (for tests/readability).
migrateMediaItem :: MediaItemV0 -> MediaItem
migrateMediaItem = migrate

instance ToJSON MediaItem where
  toJSON i = object
    [ "id"       .= itemId i
    , "title"    .= itemTitle i
    , "path"     .= itemPath i
    , "size"     .= itemSize i
    , "mime"     .= itemMime i
    , "addedAt"  .= itemAddedAt i
    , "folderId" .= itemFolder i
    ]
