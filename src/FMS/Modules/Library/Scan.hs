module FMS.Modules.Library.Scan
  ( scanDirectory
  , isMediaFile
  , mimeFor
  , mediaExtensions
  ) where

import Control.Monad (forM)
import Data.Char (toLower)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory
  ( doesDirectoryExist
  , doesFileExist
  , getFileSize
  , getModificationTime
  , listDirectory
  )
import System.FilePath (takeBaseName, takeExtension, (</>))

import FMS.Core.Types.MediaItem (MediaItem(..))

-- | Extensions recognised as media files (lowercase, with dot).
mediaExtensions :: [String]
mediaExtensions =
  [ ".mp4", ".mkv", ".avi", ".webm", ".mov"
  , ".mp3", ".flac", ".wav", ".ogg", ".m4a"
  ]

-- | Is this path a media file, judging by extension?
isMediaFile :: FilePath -> Bool
isMediaFile p = map toLower (takeExtension p) `elem` mediaExtensions

-- | Mime type for a path, from its extension (docs/modules.org scanner).
mimeFor :: FilePath -> Text
mimeFor p = case map toLower (takeExtension p) of
  ".mp4"  -> "video/mp4"
  ".mkv"  -> "video/x-matroska"
  ".avi"  -> "video/x-msvideo"
  ".webm" -> "video/webm"
  ".mov"  -> "video/quicktime"
  ".mp3"  -> "audio/mpeg"
  ".flac" -> "audio/flac"
  ".wav"  -> "audio/wav"
  ".ogg"  -> "audio/ogg"
  ".m4a"  -> "audio/mp4"
  _       -> "application/octet-stream"

-- | Recursively walk a directory and return one 'MediaItem' per media file.
-- The 'itemId' is left empty ("") — the acid-state 'RegisterItem' event
-- assigns the stable id when the item is registered. A missing directory
-- yields an empty list.
scanDirectory :: FilePath -> IO [MediaItem]
scanDirectory root = do
  exists <- doesDirectoryExist root
  if not exists then pure [] else go root
  where
    go dir = do
      entries <- sort <$> listDirectory dir
      fmap concat $ forM entries $ \entry -> do
        let p = dir </> entry
        isDir <- doesDirectoryExist p
        if isDir
          then go p
          else do
            isFile <- doesFileExist p
            if isFile && isMediaFile p
              then do
                size  <- getFileSize p
                mtime <- getModificationTime p
                pure [ MediaItem
                  { itemId      = ""
                  , itemTitle   = T.pack (takeBaseName p)
                  , itemPath    = p
                  , itemSize    = size
                  , itemMime    = mimeFor p
                  , itemAddedAt = mtime
                  } ]
              else pure []
