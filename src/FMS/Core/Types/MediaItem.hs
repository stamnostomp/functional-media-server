{-# LANGUAGE TemplateHaskell #-}

module FMS.Core.Types.MediaItem
  ( MediaItem(..)
  ) where

import Data.Aeson (ToJSON(..), object, (.=))
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

-- | One library item (docs/modules.org). The 'itemId' is a stable id
-- assigned by acid-state (see "FMS.Modules.Library.State"), NOT a hash of
-- the file path — that was the v1 limitation fixed in milestone 2
-- (docs/acid-state.org).
data MediaItem = MediaItem
  { itemId      :: !Text      -- ^ stable id, assigned by 'RegisterItem'
  , itemTitle   :: !Text      -- ^ base name of the file, without extension
  , itemPath    :: !FilePath  -- ^ path on disk
  , itemSize    :: !Integer   -- ^ size in bytes
  , itemMime    :: !Text      -- ^ mime type from the extension map
  , itemAddedAt :: !UTCTime   -- ^ file modification time
  } deriving (Show, Eq)

-- | SafeCopy so 'MediaItem' can live inside acid-state events and state.
deriveSafeCopy 0 'base ''MediaItem

instance ToJSON MediaItem where
  toJSON i = object
    [ "id"      .= itemId i
    , "title"   .= itemTitle i
    , "path"    .= itemPath i
    , "size"    .= itemSize i
    , "mime"    .= itemMime i
    , "addedAt" .= itemAddedAt i
    ]
