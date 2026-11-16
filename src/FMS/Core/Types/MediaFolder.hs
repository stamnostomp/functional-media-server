module FMS.Core.Types.MediaFolder
  ( MediaFolder(..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.!=), (.=))
import Data.Text (Text)

-- | A named library root (docs/todo.org section 2, milestone 2c;
-- Jellyfin /Library/MediaFolders). The library scans every folder and tags
-- each item with its 'folderId' ('itemFolder'). Persisted as JSON at
-- @stateDir/folders.json@ (see "FMS.Modules.Library.Folders"), not in
-- acid-state.
data MediaFolder = MediaFolder
  { folderId   :: !Text     -- ^ stable id ("1" is the default folder)
  , folderName :: !Text
  , folderPath :: !FilePath
  , folderType :: !Text     -- ^ "movies" | "music" | "mixed" | ... (free text, default "mixed")
  } deriving (Show, Eq)

-- | API JSON with Jellyfin-style PascalCase keys.
instance ToJSON MediaFolder where
  toJSON f = object
    [ "Id"   .= folderId f
    , "Name" .= folderName f
    , "Path" .= folderPath f
    , "Type" .= folderType f
    ]

instance FromJSON MediaFolder where
  parseJSON = withObject "MediaFolder" $ \o ->
    MediaFolder
      <$> o .:  "Id"
      <*> o .:  "Name"
      <*> o .:  "Path"
      <*> o .:? "Type" .!= "mixed"
