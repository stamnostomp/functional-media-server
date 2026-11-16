module FMS.Modules.Sessions.Monad
  ( MonadSessions (..)
  , sessionsStateKey
  , sessionStaleSeconds
  , initSessionsState
  , upsertSession
  , expireSessions
  , newSessionId
  ) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar)
import Control.Monad.IO.Class (liftIO)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, diffUTCTime, getCurrentTime)
import System.Random (newStdGen, randomRs)

import FMS.Core.Monad (AppM, getState, putState)
import FMS.Core.Types.User (User (..))
import FMS.Modules.Auth.Monad (MonadAuth (getUser))
import FMS.Modules.Sessions.Types (PlaySession (..))

-- | Namespaced key under which the sessions module stashes its
-- @TVar (HashMap Text PlaySession)@ in the typed registry
-- (docs/modules.org: the core never knows module state).
sessionsStateKey :: Text
sessionsStateKey = "sessions.active"

-- | A session whose last ping is older than this many seconds is stale
-- and dropped from the active list (Jellyfin expires sessions similarly).
sessionStaleSeconds :: Double
sessionStaleSeconds = 90

-- | Capability for live playback session reporting
-- (docs/todo.org section 6, milestone 6c). Handlers require
-- @MonadSessions m => ...@. Stats stay separate: the stream module
-- already records plays/progress in "FMS.Modules.Stats"; sessions only
-- track who is playing what right now.
class Monad m => MonadSessions m where
  startSession    :: Text -> Maybe Text -> Double -> m PlaySession
    -- ^ itemId, userId (when the request carried a valid token), initial position
  pingSession     :: Text -> m (Maybe PlaySession)
    -- ^ sessionId -> the session with LastPingAt bumped, or Nothing
  progressSession :: Text -> Double -> Bool -> m (Maybe PlaySession)
    -- ^ sessionId, positionSeconds, isPaused
  stopSession     :: Text -> Maybe Double -> m ()
    -- ^ sessionId, final position (tracked only; not forwarded to stats)
  listSessions    :: m [PlaySession]
    -- ^ active sessions, stale ones expired first, oldest first

instance MonadSessions AppM where
  startSession itemId mUserId pos = do
    now <- liftIO getCurrentTime
    sid <- liftIO newSessionId
    mUser <- case mUserId of
      Nothing  -> pure Nothing
      Just uid -> getUser uid
    let s = PlaySession
          { psId              = sid
          , psItemId          = itemId
          , psUserId          = mUserId
          , psUserName        = userName <$> mUser
          , psPositionSeconds = pos
          , psIsPaused        = False
          , psStartedAt       = now
          , psLastPingAt      = now
          }
    modifySessions (upsertSession s)
    pure s

  pingSession sid = do
    now <- liftIO getCurrentTime
    tv  <- sessionsTVar
    liftIO $ atomically $ do
      mS <- HM.lookup sid <$> readTVar tv
      case mS of
        Nothing -> pure Nothing
        Just s  -> do
          let s' = s { psLastPingAt = now }
          modifyTVar' tv (HM.insert sid s')
          pure (Just s')

  progressSession sid pos paused = do
    now <- liftIO getCurrentTime
    tv  <- sessionsTVar
    liftIO $ atomically $ do
      mS <- HM.lookup sid <$> readTVar tv
      case mS of
        Nothing -> pure Nothing
        Just s  -> do
          let s' = s { psPositionSeconds = pos
                     , psIsPaused        = paused
                     , psLastPingAt      = now
                     }
          modifyTVar' tv (HM.insert sid s')
          pure (Just s')

  stopSession sid _mFinalPos = modifySessions (HM.delete sid)

  listSessions = do
    now <- liftIO getCurrentTime
    tv  <- sessionsTVar
    liftIO $ atomically $ do
      modifyTVar' tv (expireSessions now sessionStaleSeconds)
      sortOn (\s -> (psStartedAt s, psId s)) . HM.elems <$> readTVar tv

-- | Pure: insert or replace the session (keyed by its id).
upsertSession :: PlaySession -> HashMap Text PlaySession -> HashMap Text PlaySession
upsertSession s = HM.insert (psId s) s

-- | Pure: drop sessions whose last ping is OLDER than @staleSeconds@
-- before @now@ (age > limit is stale; age exactly at the limit, or a
-- clock-skewed negative age, is kept).
expireSessions
  :: UTCTime -> Double -> HashMap Text PlaySession -> HashMap Text PlaySession
expireSessions now staleSeconds =
  HM.filter (\s -> realToFrac (diffUTCTime now (psLastPingAt s)) <= staleSeconds)

-- | A fresh session id: 16 random hex chars (same minting style as the
-- playback module's play session ids).
newSessionId :: IO Text
newSessionId = do
  g <- newStdGen
  let rs = take 16 (randomRs (0, 15) g) :: [Int]
  pure (T.pack (map (\r -> "0123456789abcdef" !! r) rs))

-- | Module init: create the empty sessions TVar under 'sessionsStateKey'.
initSessionsState :: AppM ()
initSessionsState = do
  tv <- liftIO (newTVarIO (HM.empty :: HashMap Text PlaySession))
  putState sessionsStateKey tv

-- Internals ---------------------------------------------------------------

sessionsTVar :: AppM (TVar (HashMap Text PlaySession))
sessionsTVar = do
  m <- getState sessionsStateKey
  case m of
    Just tv -> pure tv
    Nothing -> do
      tv <- liftIO (newTVarIO (HM.empty :: HashMap Text PlaySession))
      putState sessionsStateKey tv
      pure tv

modifySessions :: (HashMap Text PlaySession -> HashMap Text PlaySession) -> AppM ()
modifySessions f = do
  tv <- sessionsTVar
  liftIO (atomically (modifyTVar' tv f))
