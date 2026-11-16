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
import System.FilePath ((</>), takeDirectory)

import FMS.Core.Monad (AppEnv (..), AppM, MonadLog (..), getState, putState)
import FMS.Core.Types.Config (Config (..))
import FMS.Core.Types.MediaItem (MediaItem (..))
import FMS.Modules.Library (MonadLibrary (..))
import FMS.Modules.Metadata.Api.Tmdb (tmdbSearchMovie)
import FMS.Modules.Metadata.Api.Types (TmdbResult (..))
import FMS.Modules.Metadata.Nfo (nfoPathFor, parseMovieNfo, parseTvShowNfo)
import FMS.Modules.Metadata.Persistence (loadMetadata, saveMetadata)
import FMS.Modules.Metadata.Types (MediaMetadata (..))

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
  listAllMetadata :: m [MediaMetadata]   -- ^ every stored metadata record (milestone 10a /Genres)

instance MonadMetadata AppM where
  getMetadata itemIdent = do
    mTv <- getState metadataStateKey
    case mTv of
      Just tv -> liftIO (HM.lookup itemIdent <$> readTVarIO tv)
      Nothing -> pure Nothing

  listAllMetadata = do
    tv <- ensureMetadataTVar
    liftIO (HM.elems <$> readTVarIO tv)

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
        -- Local NFO always wins when it exists and parses (milestone 4b).
        nfoMeta <- readNfoMetadata item
        case nfoMeta of
          Just meta -> do
            setMetadata itemIdent meta
            pure (Just meta)
          -- Milestone 4c: no usable NFO -> remote provider fallback (TMDB),
          -- keyed by the item's title, only when an API key is configured.
          Nothing -> refreshFromTmdb itemIdent item

-- | Read and parse the item's NFO sidecar. 'Nothing' when the file is
-- missing, unreadable, or malformed (a malformed file is logged, then the
-- caller falls through to the remote provider). Milestone 4d: episode
-- items also accept the series-level @tvshow.nfo@ in the parent directory
-- ('parseTvShowNfo') when no per-file NFO exists.
readNfoMetadata :: MediaItem -> AppM (Maybe MediaMetadata)
readNfoMetadata item = do
  let nfoPath = nfoPathFor (itemPath item)
  exists <- liftIO (doesFileExist nfoPath)
  if exists
    then do
      result <- liftIO (try (BS.readFile nfoPath) :: IO (Either SomeException BS.ByteString))
      case result of
        Left _ -> pure Nothing
        Right bytes ->
          case parseMovieNfo bytes of
            Left err -> do
              logErr ("metadata: cannot parse " <> T.pack nfoPath <> ": " <> T.pack err)
              pure Nothing
            Right meta -> pure (Just meta)
    else if itemType item == "Episode"
      then readTvShowNfo (takeDirectory (itemPath item) </> "tvshow.nfo")
      else pure Nothing

-- | Milestone 4d: read + parse a series-level @tvshow.nfo@.
readTvShowNfo :: FilePath -> AppM (Maybe MediaMetadata)
readTvShowNfo nfoPath = do
  exists <- liftIO (doesFileExist nfoPath)
  if not exists
    then pure Nothing
    else do
      result <- liftIO (try (BS.readFile nfoPath) :: IO (Either SomeException BS.ByteString))
      case result of
        Left _ -> pure Nothing
        Right bytes ->
          case parseTvShowNfo bytes of
            Left err -> do
              logErr ("metadata: cannot parse " <> T.pack nfoPath <> ": " <> T.pack err)
              pure Nothing
            Right meta -> pure (Just meta)

-- | Milestone 4c (docs/roadmap.org milestone 4, docs/todo.org section 3):
-- the TMDB fallback for 'refreshMetadata'. When no NFO exists (or it does
-- not parse) and a TMDB API key is configured, search by the item title
-- and cache the first hit with 'metaSource' = @\"tmdb\"@. Empty results or
-- any TMDB error are logged and yield 'Nothing' — never throw, so the
-- router keeps mapping this to a graceful 404.
refreshFromTmdb :: Text -> MediaItem -> AppM (Maybe MediaMetadata)
refreshFromTmdb itemIdent item = do
  cfg <- asks envConfig
  let apiKey = configTmdbApiKey cfg
  if T.null apiKey
    then pure Nothing
    else do
      res <- liftIO (tmdbSearchMovie apiKey (itemTitle item))
      case res of
        Left err -> do
          logErr ("metadata: TMDB search for \"" <> itemTitle item <> "\" failed: " <> T.pack err)
          pure Nothing
        Right [] -> do
          logInfo ("metadata: TMDB has no results for \"" <> itemTitle item <> "\"")
          pure Nothing
        Right (top : _) -> do
          let meta = tmdbResultToMetadata top
          setMetadata itemIdent meta
          logInfo ("metadata: \"" <> itemTitle item <> "\" matched TMDB id "
                   <> T.pack (show (tmdbId top)) <> " (\"" <> tmdbTitle top <> "\")")
          pure (Just meta)

-- | Map a TMDB search hit onto the stored 'MediaMetadata' shape. Fields
-- TMDB search results do not carry (genres, runtime) stay empty; the id
-- goes into 'metaProviderIds' under @\"tmdb\"@.
tmdbResultToMetadata :: TmdbResult -> MediaMetadata
tmdbResultToMetadata r =
  MediaMetadata
    { metaTitle          = Just (tmdbTitle r)
    , metaOverview       = tmdbOverview r
    , metaGenres         = []
    , metaYear           = tmdbReleaseYear r
    , metaRating         = tmdbVoteAverage r
    , metaRuntimeMinutes = Nothing
    , metaProviderIds    = HM.singleton "tmdb" (T.pack (show (tmdbId r)))
    , metaSource         = "tmdb"
    }

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
