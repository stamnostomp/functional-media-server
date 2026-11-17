module FMS.Core.Monad
  ( AppError(..)
  , AppEnv(..)
  , newAppEnv
  , AppM(..)
  , runApp
  , MonadLog(..)
  , putState
  , getState
  , statusForError
  , errorMessage
  , acquireFfmpegSlot
  , releaseFfmpegSlot
  , ffmpegSlotGraceSec
  ) where

import Control.Concurrent.QSemN (QSemN, signalQSemN)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO)
import Control.Exception (SomeException, catch)
import Control.Monad (when)
import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, runReaderT)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Typeable (Typeable)
import System.IO (hPutStrLn, stderr)

import FMS.Core.Process (ffmpegSlotsKey, tryAcquireSlot)
import FMS.Core.Types.Config (Config (..))
import FMS.Core.Types.Log (LogEntry (..), logBufferCap, logLevelAllows, pushLogEntry)

-- | Application errors; the router maps these to HTTP 400 / 401 / 403 /
-- 404 / 429 / 500 (see docs/modules.org).
data AppError
  = NotFound Text
  | BadRequest Text
  | Unauthorized Text
  | Forbidden Text      -- ^ milestone 5e; maps to HTTP 403
  | TooManyRequests Text  -- ^ milestone 14a rate limiting; maps to HTTP 429
  | ServiceUnavailable Text -- ^ milestone 15 ffmpeg slot exhaustion; maps to HTTP 503
  | InternalError Text
  deriving (Show, Eq)

-- | Milestone (error model): the HTTP status code of an 'AppError'.
-- Pure, exported for tests and for the router's 'errorBody'.
statusForError :: AppError -> Int
statusForError e = case e of
  BadRequest _     -> 400
  Unauthorized _   -> 401
  Forbidden _      -> 403
  NotFound _       -> 404
  TooManyRequests _ -> 429
  ServiceUnavailable _ -> 503
  InternalError _  -> 500

-- | Milestone (error model): the human-readable message of an 'AppError'.
-- Pure, exported so the router renders one consistent body shape.
errorMessage :: AppError -> Text
errorMessage e = case e of
  BadRequest m     -> m
  Unauthorized m   -> m
  Forbidden m      -> m
  NotFound m       -> m
  TooManyRequests m -> m
  ServiceUnavailable m -> m
  InternalError m  -> m

-- | Read-only environment plus the typed state registry that modules use to
-- stash their own state under namespaced keys.
data AppEnv = AppEnv
  { envConfig :: !Config
  , envState  :: !(IORef (HashMap Text Dynamic))
  }

newAppEnv :: Config -> IO AppEnv
newAppEnv cfg = AppEnv cfg <$> newIORef HashMap.empty

-- | The application monad: read-only 'AppEnv', short-circuiting 'AppError',
-- IO at the base.
newtype AppM a = AppM (ReaderT AppEnv (ExceptT AppError IO) a)
  deriving newtype
    ( Functor, Applicative, Monad, MonadIO
    , MonadReader AppEnv, MonadError AppError
    )

-- | Run a handler, catching any exception into 'InternalError' so one bad
-- request never kills the server.
runApp :: AppEnv -> AppM a -> IO (Either AppError a)
runApp env (AppM m) =
  runExceptT (runReaderT m env)
    `catch` \e -> pure (Left (InternalError (T.pack (show (e :: SomeException)))))

-- | Minimal logging capability for modules.
class Monad m => MonadLog m where
  logInfo :: Text -> m ()
  logErr  :: Text -> m ()

-- | Milestone 11b: the AppM logging instance appends every entry to the
-- "system.logbuf" ring TVar (read by @GET /System/Logs/Log@) and only
-- prints to stdout/stderr when 'logLevelAllows' (configLogLevel) passes.
instance MonadLog AppM where
  logInfo t = logWith "info" t
  logErr  t = logWith "error" t

logWith :: Text -> Text -> AppM ()
logWith lvl t = do
  cfg <- asks envConfig
  now <- liftIO getCurrentTime
  tv  <- logBufTVar
  liftIO (atomically (modifyTVar' tv (\buf -> pushLogEntry logBufferCap buf (LogEntry now lvl t))))
  when (logLevelAllows (configLogLevel cfg) lvl) $
    liftIO $ case lvl of
      "error" -> hPutStrLn stderr ("[ERROR] " ++ T.unpack t)
      _       -> putStrLn ("[" ++ T.unpack (T.toUpper lvl) ++ "] " ++ T.unpack t)

-- | The "system.logbuf" ring TVar, created lazily on first use (the
-- ensureUserDataTVar pattern in FMS.Modules.UserData.Monad).
logBufTVar :: AppM (TVar [LogEntry])
logBufTVar = do
  m <- getState "system.logbuf"
  case m of
    Just tv -> pure tv
    Nothing -> do
      tv <- liftIO (newTVarIO [])
      putState "system.logbuf" tv
      pure tv

-- | Stash a value in the typed state registry under a namespaced key,
-- e.g. @putState \"library.index\" index@.
putState :: Typeable a => Text -> a -> AppM ()
putState key v = do
  env <- ask
  liftIO (atomicModifyIORef' (envState env) (\m -> (HashMap.insert key (toDyn v) m, ())))

-- | Read a value back from the typed state registry.
getState :: Typeable a => Text -> AppM (Maybe a)
getState key = do
  env <- ask
  m <- liftIO (readIORef (envState env))
  pure (HashMap.lookup key m >>= fromDynamic)

-- Milestone 15 (ffmpeg process management, docs/todo.org section 13) ------

-- | Grace period in seconds when waiting for a free ffmpeg slot before
-- answering 503 (polled via 'tryAcquireSlot').
ffmpegSlotGraceSec :: Int
ffmpegSlotGraceSec = 5

-- | Take one unit of the global ffmpeg slot semaphore (registry key
-- 'ffmpegSlotsKey', created in @runServer@ with 'configMaxTranscodes'
-- units). Returns 'Just sem' when a slot was acquired and must be
-- released with 'releaseFfmpegSlot' when the job ends, or 'Nothing'
-- when no semaphore is registered (unit tests): the job then runs
-- unthrottled. Throws 'ServiceUnavailable' (HTTP 503) when no slot frees
-- up within 'ffmpegSlotGraceSec' seconds.
acquireFfmpegSlot :: AppM (Maybe QSemN)
acquireFfmpegSlot = do
  mSem <- getState ffmpegSlotsKey
  case mSem of
    Nothing  -> pure Nothing
    Just sem -> do
      ok <- liftIO (tryAcquireSlot sem ffmpegSlotGraceSec)
      if ok
        then pure (Just sem)
        else throwError (ServiceUnavailable "too many concurrent transcodes")

-- | Release a slot taken with 'acquireFfmpegSlot' ('Nothing' is a no-op).
-- Plain IO so it can run in the streaming body's cleanup (a client
-- disconnect mid-transcode must still free the slot).
releaseFfmpegSlot :: Maybe QSemN -> IO ()
releaseFfmpegSlot = maybe (pure ()) (`signalQSemN` 1)
