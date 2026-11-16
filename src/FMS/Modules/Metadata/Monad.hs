module FMS.Modules.Metadata.Monad
  ( MonadMetadata (..)
  , metadataStateKey
  , metadataFilePath
  , loadMetadataIntoState
  ) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO)
import Control.Exception (SomeException, try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import qualified Data.ByteString as BS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))

import FMS.Core.Monad (AppEnv (..), AppM, MonadLog (..), getState, putState)
import FMS.Core.Types.Config (Config (..))
import FMS.Core.Types.MediaItem (MediaItem (..))
import FMS.Modules.Library (MonadLibrary (..))
import FMS.Modules.Metadata.Nfo (nfoPathFor, parseMovieNfo)
import FMS.Modules.Metadata.Persistence (loadMetadata, saveMetadata)
import FMS.Modules.Metadata.Types (MediaMetadata)

-- | Namespaced key under which the metadata module stashes its
-- @TVar (HashMap Text MediaMetadata)@ in the typed registry
-- (docs/modules.org: the core never knows module state).
metadataStateKey :: Text
metadataStateKey = "metadata.cache"

-- | Where the metadata map is persisted: @stateDir/metadata.json@.
metadataFilePath :: Config -> FilePath
metadataFilePath cfg = configStateDir cfg </> "metadata.json"

-- | Capability for reading and refreshing item metadata
-- (docs/roadmap.org milestone 4, docs/todo.org section 3). Handlers in
-- other modules can require @MonadMetadata m => ...@.
class Monad m => MonadMetadata m where
  getMetadata     :: Text -> m (Maybe MediaMetadata)
  setMetadata     :: Text -> MediaMetadata -> m ()
  refreshMetadata :: Text -> m (Maybe MediaMetadata)

instance MonadMetadata AppM where
  getMetadata itemIdent = do
    mTv <- getState metadataStateKey
    case mTv of
      Just tv -> liftIO (HM.lookup itemIdent <$> readTVarIO tv)
      Nothing -> pure Nothing

  setMetadata itemIdent meta = do
    cfg <- asks envConfig
    tv <- ensureMetadataTVar
    -- Persist after every update: in-memory TVar for reads, JSON on disk
    -- for restarts (the Stats module pattern).
    updated <- liftIO $ atomically $ do
      modifyTVar' tv (HM.insert itemIdent meta)
      readTVar tv
    liftIO (saveMetadata (metadataFilePath cfg) updated)

  refreshMetadata itemIdent = do
    mItem <- getItem itemIdent
    case mItem of
      Nothing -> pure Nothing
      Just item -> do
        let nfoPath = nfoPathFor (itemPath item)
        exists <- liftIO (doesFileExist nfoPath)
        if not exists
          then pure Nothing
          else do
            result <- liftIO (try (BS.readFile nfoPath) :: IO (Either SomeException BS.ByteString))
            case result of
              Left _ -> pure Nothing
              Right bytes ->
                case parseMovieNfo bytes of
                  Left err -> do
                    logErr ("metadata: cannot parse " <> T.pack nfoPath <> ": " <> T.pack err)
                    pure Nothing
                  Right meta -> do
                    setMetadata itemIdent meta
                    pure (Just meta)
        -- SEAM for remote providers (milestone 4c, TMDB/TvMaze): when no
        -- local NFO exists (or it fails to parse), a remote lookup keyed
        -- by the item's title/year (or an existing metaProviderIds entry)
        -- would go here instead of returning Nothing.

-- | The metadata TVar, creating and registering it on first use (the
-- Stats module's ensureStatsTVar pattern).
ensureMetadataTVar :: AppM (TVar (HashMap Text MediaMetadata))
ensureMetadataTVar = do
  mTv <- getState metadataStateKey
  case mTv of
    Just tv -> pure tv
    Nothing -> do
      tv <- liftIO (newTVarIO HM.empty)
      putState metadataStateKey tv
      pure tv

-- | Reads the persisted metadata file into a fresh TVar under
-- 'metadataStateKey' (creating the state dir if needed). Called once
-- from the metadata module's @moduleInit@.
loadMetadataIntoState :: AppM ()
loadMetadataIntoState = do
  cfg <- asks envConfig
  liftIO (createDirectoryIfMissing True (configStateDir cfg))
  loaded <- liftIO (loadMetadata (metadataFilePath cfg))
  tv <- liftIO (newTVarIO loaded)
  putState metadataStateKey tv
