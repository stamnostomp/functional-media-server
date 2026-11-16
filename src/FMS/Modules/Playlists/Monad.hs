module FMS.Modules.Playlists.Monad
  ( MonadPlaylists (..)
  , playlistsStateKey
  , playlistsFilePath
  , loadPlaylistsIntoState
  , addItem
  , removeItem
  , newPlaylistId
  ) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Random (newStdGen, randomRs)

import FMS.Core.Monad (AppM, MonadLog (..), envConfig, getState, putState)
import FMS.Core.Types.Config (Config (..))
import FMS.Core.Types.MediaItem (MediaItem (..))
import FMS.Modules.Library (MonadLibrary (..))
import FMS.Modules.Playlists.Persistence (loadPlaylists, savePlaylists)
import FMS.Modules.Playlists.Types (Playlist (..))

-- | Namespaced key under which the playlists module stashes its
-- @TVar (HashMap Text Playlist)@ in the typed registry
-- (docs/modules.org: the core never knows module state).
playlistsStateKey :: Text
playlistsStateKey = "playlists.items"

-- | Where the playlist map is persisted: @stateDir/playlists.json@.
playlistsFilePath :: Config -> FilePath
playlistsFilePath cfg = configStateDir cfg </> "playlists.json"

-- | Capability for managing playlists (docs/todo.org section 10,
-- milestone 10a). The @Bool@ results of the mutators mean "the playlist
-- existed and was changed" — the routes map 'False' to 404.
class Monad m => MonadPlaylists m where
  listPlaylists      :: m [Playlist]                        -- ^ all playlists, oldest first
  getPlaylist        :: Text -> m (Maybe Playlist)          -- ^ by id
  createPlaylist     :: Text -> [Text] -> m Playlist        -- ^ name, item ids (unknown ids dropped)
  deletePlaylist     :: Text -> m Bool                      -- ^ id -> existed?
  addToPlaylist      :: Text -> Text -> m Bool              -- ^ playlist id, item id -> changed?
  removeFromPlaylist :: Text -> Text -> m Bool              -- ^ playlist id, item id -> changed?

instance MonadPlaylists AppM where
  listPlaylists = do
    tv <- playlistsTVar
    sortOn (\p -> (plCreatedAt p, plId p)) . HM.elems <$> liftIO (readTVarIO tv)

  getPlaylist pid = do
    tv <- playlistsTVar
    liftIO (HM.lookup pid <$> readTVarIO tv)

  createPlaylist name itemIds = do
    -- Unknown item ids are dropped silently (but logged); the valid ones
    -- are kept in the given order.
    allItems <- listItems
    let known = HM.fromList [(itemId i, ()) | i <- allItems]
        valid   = filter (\i -> HM.member i known) itemIds
        dropped = filter (\i -> not (HM.member i known)) itemIds
    unless (null dropped) $
      logInfo ("playlists: dropping unknown item id(s) from new playlist: "
               <> T.intercalate ", " dropped)
    now <- liftIO getCurrentTime
    pid <- liftIO newPlaylistId
    let p = Playlist pid name valid now
    _ <- modifyPlaylists (HM.insert pid p)
    pure p

  deletePlaylist pid = do
    mP <- getPlaylist pid
    case mP of
      Nothing -> pure False
      Just _  -> do
        _ <- modifyPlaylists (HM.delete pid)
        pure True

  addToPlaylist pid iid = do
    mP <- getPlaylist pid
    case mP of
      Nothing -> pure False
      Just p  -> do
        _ <- modifyPlaylists (HM.insert pid (p { plItemIds = addItem iid (plItemIds p) }))
        pure True

  removeFromPlaylist pid iid = do
    mP <- getPlaylist pid
    case mP of
      Nothing -> pure False
      Just p  -> do
        _ <- modifyPlaylists (HM.insert pid (p { plItemIds = removeItem iid (plItemIds p) }))
        pure True

-- | Pure: append @x@ to the item list unless already present (dedupe,
-- order preserved).
addItem :: Text -> [Text] -> [Text]
addItem x xs = if x `elem` xs then xs else xs ++ [x]

-- | Pure: remove every occurrence of @x@ from the item list.
removeItem :: Text -> [Text] -> [Text]
removeItem x = filter (/= x)

-- | A fresh playlist id: 16 random hex chars (same minting style as the
-- sessions module's session ids and the auth module's tokens).
newPlaylistId :: IO Text
newPlaylistId = do
  g <- newStdGen
  let rs = take 16 (randomRs (0, 15) g) :: [Int]
  pure (T.pack (map (\r -> "0123456789abcdef" !! r) rs))

-- | Module init: load the persisted @playlists.json@ into a fresh TVar
-- under 'playlistsStateKey' (creating the state dir if needed).
loadPlaylistsIntoState :: AppM ()
loadPlaylistsIntoState = do
  cfg <- asks envConfig
  liftIO (createDirectoryIfMissing True (configStateDir cfg))
  loaded <- liftIO (loadPlaylists (playlistsFilePath cfg))
  tv <- liftIO (newTVarIO loaded)
  putState playlistsStateKey tv

-- Internals ---------------------------------------------------------------

playlistsTVar :: AppM (TVar (HashMap Text Playlist))
playlistsTVar = do
  m <- getState playlistsStateKey
  case m of
    Just tv -> pure tv
    Nothing -> do
      tv <- liftIO (newTVarIO (HM.empty :: HashMap Text Playlist))
      putState playlistsStateKey tv
      pure tv

-- | Apply an update to the playlists TVar AND persist the result to
-- @stateDir/playlists.json@ after every mutation (the Stats/Metadata
-- module pattern: in-memory for reads, JSON on disk for restarts).
modifyPlaylists
  :: (HashMap Text Playlist -> HashMap Text Playlist)
  -> AppM (HashMap Text Playlist)
modifyPlaylists f = do
  cfg <- asks envConfig
  tv  <- playlistsTVar
  updated <- liftIO $ atomically $ do
    modifyTVar' tv f
    readTVar tv
  liftIO (savePlaylists (playlistsFilePath cfg) updated)
  pure updated
