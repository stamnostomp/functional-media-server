module FMS.Modules.Metadata.Monad
  ( MonadMetadata (..)
  , metadataStateKey
  , metadataFilePath
  , loadMetadataIntoState
  , refreshAllMetadata
  , metadataSettingsStateKey
  , metadataSettingsFilePath
  , loadMetadataSettingsIntoState
  , getMetadataSettings
  , updateMetadataSettings
  , refreshFromTmdb
  ) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO)
import Control.Exception (SomeException, try)
import Control.Monad (foldM, forM_, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import qualified Data.ByteString as BS
import Data.HashMap.Strict (HashMap)
import Data.Maybe (isJust)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), takeDirectory, takeExtension)

import FMS.Core.Monad (AppEnv (..), AppM, MonadLog (..), getState, putState)
import FMS.Core.Types.Config (Config (..))
import FMS.Core.Types.MediaItem (MediaItem (..))
import FMS.Modules.Images (hasStoredImage, storeImageBytes)
import FMS.Modules.Library.Monad (MonadLibrary (..))
import FMS.Modules.Metadata.Api.Images (fetchTmdbImages)
import FMS.Modules.Metadata.Api.Tmdb (tmdbMovieDetailsAt, tmdbSearchMovieAt, tmdbSearchTvAt, tmdbTvDetailsKwAt)
import FMS.Modules.Metadata.Api.Types (TmdbResult (..), cleanTitleForProvider, pickTmdbResult)
import FMS.Modules.Metadata.Nfo (nfoPathFor, parseMovieNfo, parseTvShowNfo)
import FMS.Modules.Metadata.Persistence (loadMetadata, loadMetadataSettings, saveMetadata, saveMetadataSettings)
import FMS.Modules.Metadata.Types (MediaMetadata (..), MetadataSettings (..), applyProviderMerge, emptyMetadata, metadataSettingsFromConfig)

-- | Namespaced key under which the metadata module stashes its
-- @TVar (HashMap Text MediaMetadata)@ in the typed registry
-- (docs/modules.org: the core never knows module state).
metadataStateKey :: Text
metadataStateKey = "metadata.cache"

-- | Where the metadata map is persisted: @stateDir/metadata.json@.
metadataFilePath :: Config -> FilePath
metadataFilePath cfg = configStateDir cfg </> "metadata.json"

-- | m35: namespaced key for the TMDB settings TVar.
metadataSettingsStateKey :: Text
metadataSettingsStateKey = "metadata.settings"

-- | Where the TMDB settings are persisted: @stateDir/metadata-settings.json@.
metadataSettingsFilePath :: Config -> FilePath
metadataSettingsFilePath cfg = configStateDir cfg </> "metadata-settings.json"

-- | m35: load the persisted @metadata-settings.json@; when absent (first
-- boot) seed from the env-derived config and save. When present, the saved
-- values win (effective settings).
loadMetadataSettingsIntoState :: AppM ()
loadMetadataSettingsIntoState = do
  cfg <- asks envConfig
  liftIO (createDirectoryIfMissing True (configStateDir cfg))
  let path = metadataSettingsFilePath cfg
  mSaved <- liftIO (loadMetadataSettings path)
  s <- case mSaved of
    Just saved -> pure saved
    Nothing    -> do
      let seeded = metadataSettingsFromConfig cfg
      liftIO (saveMetadataSettings path seeded)
      pure seeded
  tv <- liftIO (newTVarIO s)
  putState metadataSettingsStateKey tv

-- | m35: the effective (saved, else env-seeded) TMDB settings.
getMetadataSettings :: AppM MetadataSettings
getMetadataSettings = do
  m <- getState metadataSettingsStateKey
  case m of
    Just tv -> liftIO (readTVarIO tv)
    Nothing -> do
      cfg <- asks envConfig
      let s = metadataSettingsFromConfig cfg
      tv <- liftIO (newTVarIO s)
      putState metadataSettingsStateKey tv
      pure s

-- | m35: apply an update to the settings TVar AND persist to
-- @stateDir/metadata-settings.json@.
updateMetadataSettings :: (MetadataSettings -> MetadataSettings) -> AppM MetadataSettings
updateMetadataSettings f = do
  cfg <- asks envConfig
  s0 <- getMetadataSettings
  m <- getState metadataSettingsStateKey
  case m of
    Nothing -> pure s0
    Just tv -> do
      updated <- liftIO $ atomically $ do
        modifyTVar' tv f
        readTVar tv
      liftIO (saveMetadataSettings (metadataSettingsFilePath cfg) updated)
      pure updated

-- | Capability for reading and refreshing item metadata
-- (docs/roadmap.org milestone 4, docs/todo.org section 3). Handlers in
-- other modules can require @MonadMetadata m => ...@.
class Monad m => MonadMetadata m where
  getMetadata     :: Text -> m (Maybe MediaMetadata)
  setMetadata     :: Text -> MediaMetadata -> m ()
  refreshMetadataForced :: Bool -> Text -> m (Maybe MediaMetadata)
  refreshMetadata :: Text -> m (Maybe MediaMetadata)
  refreshMetadata = refreshMetadataForced False
  listAllMetadata :: m [MediaMetadata]   -- ^ every stored metadata record (milestone 10a /Genres)
  listAllMetadataMap :: m (HashMap Text MediaMetadata)  -- ^ the whole cache keyed by itemId (milestone 4h /items filters)

instance MonadMetadata AppM where
  getMetadata itemIdent = do
    mTv <- getState metadataStateKey
    case mTv of
      Just tv -> liftIO (HM.lookup itemIdent <$> readTVarIO tv)
      Nothing -> pure Nothing

  listAllMetadata = do
    tv <- ensureMetadataTVar
    liftIO (HM.elems <$> readTVarIO tv)

  listAllMetadataMap = do
    tv <- ensureMetadataTVar
    liftIO (readTVarIO tv)

  setMetadata itemIdent meta = do
    cfg <- asks envConfig
    tv <- ensureMetadataTVar
    -- Persist after every update: in-memory TVar for reads, JSON on disk
    -- for restarts (the Stats module pattern).
    updated <- liftIO $ atomically $ do
      modifyTVar' tv (HM.insert itemIdent meta)
      readTVar tv
    liftIO (saveMetadata (metadataFilePath cfg) updated)

  refreshMetadataForced force itemIdent = do
    mItem <- getItem itemIdent
    case mItem of
      Nothing -> pure Nothing
      Just item -> do
        -- Local NFO always wins when it exists and parses (milestone 4b).
        nfoMeta <- readNfoMetadata item
        case nfoMeta of
          Just meta -> do
            merged <- mergeWithStored itemIdent meta
            setMetadata itemIdent merged
            pure (Just merged)
          -- Milestone 4c: no usable NFO -> remote provider fallback (TMDB),
          -- keyed by the item's title, only when an API key is configured.
          -- m57: force is threaded through so an explicit refresh also
          -- re-downloads artwork at the current size.
          Nothing -> refreshFromTmdb force itemIdent item

-- | Milestone 4m (docs/todo.org section 3): refresh the metadata of EVERY
-- library item sequentially (NFO first, TMDB fallback -- the same path as
-- 'refreshMetadata'). When @replaceAll@ is True every stored record is
-- reset to an empty one first, so stale provider data vanishes too --
-- except the user-curated fields (item tags 'metaTags' and the field
-- locks 'metaLockedFields'), which are kept so manual edits (the item
-- tags API, milestone 4k locking) survive a full refresh. Returns the
-- number of items for which a refresh found metadata.
refreshAllMetadata :: Bool -> AppM Int
refreshAllMetadata replaceAll = do
  cfg <- asks envConfig
  when replaceAll $ do
    tv <- ensureMetadataTVar
    liftIO (atomically (modifyTVar' tv (HM.map keepUserCurated)))
    updated <- liftIO (readTVarIO tv)
    liftIO (saveMetadata (metadataFilePath cfg) updated)
  items <- listItems
  foldM step 0 items
  where
    step :: Int -> MediaItem -> AppM Int
    step acc item = do
      m <- refreshMetadataForced True (itemId item)  -- m57: bulk refresh re-downloads artwork too
      pure (acc + (if isJust m then 1 else 0))
    -- replaceAll keeps only the user-curated fields of each record
    keepUserCurated :: MediaMetadata -> MediaMetadata
    keepUserCurated m = emptyMetadata
      { metaTags = metaTags m
      , metaLockedFields = metaLockedFields m
      }

-- | Milestone 4k: merge a freshly parsed provider record over the stored
-- one so that fields the user locked (the old record's 'metaLockedFields')
-- survive a refresh ('applyProviderMerge'). With nothing stored yet the
-- fresh record is used as-is.
mergeWithStored :: Text -> MediaMetadata -> AppM MediaMetadata
mergeWithStored itemIdent fresh = do
  mOld <- getMetadata itemIdent
  pure (maybe fresh (\old -> applyProviderMerge old fresh) mOld)

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
refreshFromTmdb :: Bool -> Text -> MediaItem -> AppM (Maybe MediaMetadata)
refreshFromTmdb force itemIdent item = do
  cfg <- asks envConfig
  ms <- getMetadataSettings          -- m35: effective (saved) settings
  let apiKey = msTmdbApiKey ms
  if T.null apiKey
    then pure Nothing
    else do
      -- m36: clean scene/release names (Shrek.2001.1080p... -> "Shrek" 2001)
      -- before searching, and prefer the hit whose year matches.
      -- m45: episodes search TMDB TV (/search/tv) with the SERIES name
      -- (itemSeries, falling back to the cleaned file title); movies keep
      -- the movie search.
      let isEp = T.toLower (itemType item) == "episode"
          rawQ = case itemSeries item of
                   Just sn | not (T.null sn) -> sn
                   _                         -> itemTitle item
          (cleanQ, mYear) = cleanTitleForProvider rawQ
          searchFn = if isEp then tmdbSearchTvAt else tmdbSearchMovieAt
          kindWord = if isEp then "tv" else "movie"
      res0 <- liftIO (searchFn (configTmdbBaseUrl cfg) apiKey cleanQ)
      res <- case res0 of
        Right [] | cleanQ /= rawQ ->
          liftIO (searchFn (configTmdbBaseUrl cfg) apiKey rawQ)
        _ -> pure res0
      case res of
        Left err -> do
          logErr ("metadata: TMDB " <> kindWord <> " search for \"" <> cleanQ <> "\" failed: " <> T.pack err)
          pure Nothing
        Right rs -> case pickTmdbResult mYear rs of
          Nothing -> do
            logInfo ("metadata: TMDB " <> kindWord <> " has no results for \"" <> cleanQ <> "\"")
            pure Nothing
          Just top -> do
            let fresh = tmdbResultToMetadata top
            merged0 <- mergeWithStored itemIdent fresh
            -- m53a: /search carries no genre ids; fetch the detail endpoint
            -- (append_to_response=keywords) for genres + keywords
            -- (failure-graceful: they just stay empty).
            let detailsFn = if isEp then tmdbTvDetailsKwAt else tmdbMovieDetailsAt
            dres <- liftIO (detailsFn (configTmdbBaseUrl cfg) apiKey (tmdbId top))
            case dres of
              Left err ->
                logInfo ("metadata: TMDB " <> kindWord <> " details for id "
                         <> T.pack (show (tmdbId top)) <> " failed: " <> T.pack err)
              Right _ -> pure ()
            let (gs, kws) = either (const ([], [])) id dres
                locked = map T.toLower (metaLockedFields merged0)
                merged = merged0
                  { metaGenres   = if "genres" `elem` locked || null gs then metaGenres merged0 else gs
                  , metaKeywords = kws
                  }
            setMetadata itemIdent merged
            logInfo ("metadata: \"" <> itemTitle item <> "\" (" <> kindWord <> " \"" <> cleanQ
                     <> "\") matched TMDB id "
                     <> T.pack (show (tmdbId top)) <> " (\"" <> tmdbTitle top <> "\")")
            -- TMDB artwork (opt-in via FMS_FETCH_IMAGES): download poster /
            -- backdrops / logo into the item image store. m57: on an explicit
            -- refresh the artwork is re-downloaded (force), so a size change
            -- (e.g. w1280 -> original backdrops) takes effect; on a plain
            -- scan an already stored (user-uploaded) image is never
            -- overwritten.
            when (msFetchImages ms) $   -- m35: effective fetch-images flag
              fetchAndStoreTmdbImages cfg force itemIdent (itemType item) (tmdbId top)
            pure (Just merged)

-- | TMDB artwork download for one item (opt-in, FMS_FETCH_IMAGES): fetch
-- the image list for the resolved TMDB id (@movie@ vs @tv@ by item type)
-- and store each image under @stateDir/images/<id>@ via
-- 'FMS.Modules.Images.storeImageBytes'. When @force@ is False an image of
-- that type that is already stored (e.g. user-uploaded) is kept; when
-- @force@ is True (explicit metadata refresh, m57) stored artwork is
-- re-downloaded so size changes take effect. Every step is
-- failure-graceful (logged, never thrown) so a scan is never crashed by
-- artwork; each HTTP call has the 10 s TMDB timeout.
fetchAndStoreTmdbImages :: Config -> Bool -> Text -> Text -> Int -> AppM ()
fetchAndStoreTmdbImages cfg force itemIdent itemTy tmdbIdNum = do
  ms <- getMetadataSettings          -- m35: effective (saved) settings
  let apiKey = msTmdbApiKey ms
  if T.null apiKey
    then pure ()
    else do
      let kind = if itemTy == "Episode" then "tv" else "movie"
          stateDir = configStateDir cfg
      r <- liftIO (try (fetchTmdbImages apiKey (configTmdbBaseUrl cfg) kind
                         (T.pack (show tmdbIdNum)) (configTmdbImageBaseUrl cfg)
                         (configTmdbImageSize cfg) (configTmdbBackdropSize cfg))  -- m49
                      :: IO (Either SomeException (Either String [(Text, Text, BS.ByteString)])))
      case r of
        Left e ->
          logErr ("metadata: TMDB image fetch for item " <> itemIdent
                  <> " failed: " <> T.pack (show e))
        Right (Left err) ->
          logInfo ("metadata: no TMDB images for item " <> itemIdent <> ": " <> T.pack err)
        Right (Right dl) ->
          forM_ dl $ \(ty, fp, bytes) -> do
            has <- liftIO (hasStoredImage stateDir itemIdent ty)
            if has && not force
              then pure ()
              else do
                w <- liftIO (try (storeImageBytes stateDir itemIdent ty
                                   (takeExtension (T.unpack fp)) bytes)
                              :: IO (Either SomeException FilePath))
                case w of
                  Left e ->
                    logErr ("metadata: cannot store " <> ty <> " for item " <> itemIdent
                            <> ": " <> T.pack (show e))
                  Right p ->
                    logInfo ("metadata: stored TMDB " <> ty <> " for item " <> itemIdent
                             <> " at " <> T.pack p)

-- | Map a TMDB search hit onto the stored 'MediaMetadata' shape. Fields
-- TMDB search results do not carry (genres, runtime) stay empty; the id
-- goes into 'metaProviderIds' under @\"tmdb\"@. Milestone 4h: studios /
-- persons / tags also stay empty here -- the search endpoint does not
-- return them (that would need the credits / production_companies
-- endpoints); NFO-sourced metadata carries them instead.
tmdbResultToMetadata :: TmdbResult -> MediaMetadata
tmdbResultToMetadata r =
  MediaMetadata
    { metaTitle          = Just (tmdbTitle r)
    , metaOverview       = tmdbOverview r
    , metaGenres         = []
    , metaKeywords       = []                          -- m53a: filled from the detail endpoint
    , metaStudios        = []
    , metaPersons        = []
    , metaTags           = []
    , metaYear           = tmdbReleaseYear r
    , metaRating         = tmdbVoteAverage r
    , metaOfficialRating = Nothing                    -- milestone 9: search results carry no <mpaa>
    , metaRuntimeMinutes = Nothing
    , metaProviderIds    = HM.singleton "tmdb" (T.pack (show (tmdbId r)))
    , metaSource         = "tmdb"
    , metaLockedFields   = []                          -- milestone 4k
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
