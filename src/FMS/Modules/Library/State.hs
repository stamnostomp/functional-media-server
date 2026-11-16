{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module FMS.Modules.Library.State
  ( LibraryState(..)
  , initialLibraryState
  , NewId(..)
  , RegisterItem(..)
  , GetItemById(..)
  , GetIdByPath(..)
  , RemoveItem(..)
  , UpdateItem(..)
  , UpdateItemMeta(..)
  , RemovePaths(..)
  , ListPaths(..)
  , GetAllItems(..)
  ) where

import Control.Monad.Reader (ask)
import Control.Monad.State (get, modify', put)
import Data.Acid (Query, Update, makeAcidic)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)

import FMS.Core.Types.MediaItem (MediaItem(..))

-- | Persistent library state (docs/acid-state.org). IDs are allocated
-- monotonically from 'nextId' and survive restarts because the whole state
-- is persisted by acid-state.
data LibraryState = LibraryState
  { pathToId :: !(Map FilePath Text)   -- ^ path -> stable id
  , idToItem :: !(Map Text MediaItem)  -- ^ id -> item
  , nextId   :: !Int                   -- ^ next id to allocate
  } deriving (Show, Eq)

deriveSafeCopy 0 'base ''LibraryState

initialLibraryState :: LibraryState
initialLibraryState = LibraryState Map.empty Map.empty 1

-- | The textual form of an id.
idFor :: Int -> Text
idFor = T.pack . show

-- | Allocate a fresh id.
newId :: Update LibraryState Text
newId = do
  st <- get
  let n = nextId st
  put st { nextId = n + 1 }
  pure (idFor n)

-- | Register a scanned file. Idempotent: if the path already has an id it
-- is returned unchanged, otherwise a fresh id is allocated and both
-- mappings are stored (docs/acid-state.org).
registerItem :: FilePath -> MediaItem -> Update LibraryState Text
registerItem path item = do
  st <- get
  case Map.lookup path (pathToId st) of
    Just existing -> pure existing
    Nothing -> do
      let n = nextId st
          i = idFor n
      put st
        { pathToId = Map.insert path i (pathToId st)
        , idToItem = Map.insert i item { itemId = i } (idToItem st)
        , nextId   = n + 1
        }
      pure i

getItemById :: Text -> Query LibraryState (Maybe MediaItem)
getItemById i = Map.lookup i . idToItem <$> ask

getIdByPath :: FilePath -> Query LibraryState (Maybe Text)
getIdByPath p = Map.lookup p . pathToId <$> ask

removeItem :: FilePath -> Update LibraryState ()
removeItem path = do
  st <- get
  case Map.lookup path (pathToId st) of
    Nothing -> pure ()
    Just i  -> put st
      { pathToId = Map.delete path (pathToId st)
      , idToItem = Map.delete i (idToItem st)
      }

updateItem :: Text -> MediaItem -> Update LibraryState ()
updateItem i item =
  modify' (\st -> st { idToItem = Map.insert i item (idToItem st) })

-- | Update the stored size/mtime of the item registered at @path@;
-- returns 'True' when the stored values actually changed (milestone 2b
-- incremental rescan: unchanged files keep their id AND their record).
updateItemMeta :: FilePath -> Integer -> UTCTime -> Update LibraryState Bool
updateItemMeta path size mtime = do
  st <- get
  case Map.lookup path (pathToId st) of
    Nothing -> pure False
    Just i ->
      case Map.lookup i (idToItem st) of
        Nothing -> pure False
        Just item
          | itemSize item == size && itemAddedAt item == mtime -> pure False
          | otherwise -> do
              put st
                { idToItem = Map.insert i
                    item { itemSize = size, itemAddedAt = mtime } (idToItem st) }
              pure True

-- | Remove every item registered at the given paths (milestone 2b:
-- files that disappeared from disk); returns how many were removed.
removePaths :: [FilePath] -> Update LibraryState Int
removePaths paths = do
  st <- get
  let step s p = case Map.lookup p (pathToId s) of
        Nothing -> s
        Just i  -> s
          { pathToId = Map.delete p (pathToId s)
          , idToItem = Map.delete i (idToItem s)
          }
      st' = foldl' step st paths
      removed = length [ () | p <- paths, Map.member p (pathToId st) ]
  put st'
  pure removed

-- | All registered paths (milestone 2b: the "existing" side of the
-- incremental rescan diff).
listPaths :: Query LibraryState [FilePath]
listPaths = Map.keys . pathToId <$> ask

getAllItems :: Query LibraryState [MediaItem]
getAllItems = Map.elems . idToItem <$> ask

makeAcidic ''LibraryState
  [ 'newId
  , 'registerItem
  , 'getItemById
  , 'getIdByPath
  , 'removeItem
  , 'updateItem
  , 'updateItemMeta
  , 'removePaths
  , 'listPaths
  , 'getAllItems
  ]
