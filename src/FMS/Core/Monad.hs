module FMS.Core.Monad
  ( AppError(..)
  , AppEnv(..)
  , newAppEnv
  , AppM(..)
  , runApp
  , MonadLog(..)
  , putState
  , getState
  ) where

import Control.Exception (SomeException, catch)
import Control.Monad.Except (ExceptT, MonadError, runExceptT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Data.Dynamic (Dynamic, fromDynamic, toDyn)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Typeable (Typeable)
import System.IO (hPutStrLn, stderr)

import FMS.Core.Types.Config (Config)

-- | Application errors; the router maps these to HTTP 400 / 404 / 500
-- (see docs/modules.org).
data AppError
  = NotFound Text
  | BadRequest Text
  | InternalError Text
  deriving (Show, Eq)

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

instance MonadLog AppM where
  logInfo t = liftIO (putStrLn ("[INFO] " ++ T.unpack t))
  logErr  t = liftIO (hPutStrLn stderr ("[ERROR] " ++ T.unpack t))

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
