module FMS.Modules.Library.Monad
  ( MonadLibrary(..)
  , libraryAcidKey
  ) where

import Control.Monad (forM_)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Acid (AcidState, query, update)
import Data.Text (Text)
import qualified Data.Text as T

import FMS.Core.Monad (AppError(..), AppM, MonadLog(..), envConfig, getState)
import FMS.Core.Types.Config (Config(..))
import FMS.Core.Types.MediaItem (MediaItem(..))
import FMS.Modules.Library.Scan (scanDirectory)
import FMS.Modules.Library.State
  ( LibraryState
  , GetAllItems(..)
  , GetItemById(..)
  , RegisterItem(..)
  )

-- | Namespaced key under which the module stashes its 'AcidState' in the
-- typed registry (docs/modules.org: the core never knows module state).
libraryAcidKey :: Text
libraryAcidKey = "library.acid"

-- | The library capability (docs/modules.org). Other modules can require
-- @MonadLibrary m => ...@ in their handlers.
class Monad m => MonadLibrary m where
  listItems     :: m [MediaItem]
  getItem       :: Text -> m (Maybe MediaItem)
  rescanLibrary :: m ()

-- | Run an acid-state action with the 'AcidState' stored in the registry by
-- the module init.
withAcid :: (AcidState LibraryState -> IO a) -> AppM a
withAcid f = do
  mAcid <- getState libraryAcidKey
  case mAcid of
    Nothing   -> throwError (InternalError (libraryAcidKey <> " not initialised"))
    Just acid -> liftIO (f acid)

instance MonadLibrary AppM where
  listItems = withAcid (`query` GetAllItems)

  getItem i = withAcid (`query` GetItemById i)

  rescanLibrary = do
    cfg <- asks envConfig
    scanned <- liftIO (scanDirectory (configLibraryPath cfg))
    forM_ scanned $ \item ->
      withAcid (`update` RegisterItem (itemPath item) item)
    logInfo (T.pack (unwords
      [ "library rescan:"
      , show (length scanned)
      , "media file(s) registered from"
      , configLibraryPath cfg
      ]))
