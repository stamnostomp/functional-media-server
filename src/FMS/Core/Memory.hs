{-# LANGUAGE CPP #-}

module FMS.Core.Memory
  ( parseProcStatusRss
  , readCurrentRssBytes
  , memSamplesKey
  , memRingCap
  , trimRing
  , startMemorySampler
  , readMemSamples
  , currentRssOrZero
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Exception (SomeException, try)
import Control.Monad (forever, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, asks)
#ifdef FMS_LINUX
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
#endif
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)

import FMS.Core.Monad (AppEnv, AppError, AppM, MonadLog (..), envConfig, getState, putState, runApp)
import FMS.Core.Types.Config (Config (..))

-- /proc/self/status is Linux-only; on other platforms
-- 'readCurrentRssBytes' returns Nothing (CPP guard, like the hinotify
-- watcher).

-- | Registry key for the memory sampler's ring buffer:
-- @TVar ([(UTCTime, Integer)], Bool)@ = (samples, oldest first, plus the
-- "warn already fired" flag), under "system.mem" (the core never knows
-- about it, docs/modules.org).
memSamplesKey :: Text
memSamplesKey = "system.mem"

-- | Ring buffer cap: keep at most this many samples.
memRingCap :: Int
memRingCap = 120

-- | Pure: parse the @VmRSS@ line of @\/proc\/self\/status@ content and
-- return the resident set size in BYTES (@VmRSS:  12345 kB@ ->
-- @Just (12345 * 1024)@). Missing or unparseable -> 'Nothing'. Exported
-- for tests.
parseProcStatusRss :: String -> Maybe Integer
parseProcStatusRss content =
  case [ l | l <- lines content, "VmRSS:" `isPrefixOfStr` l ] of
    (l : _) -> case words l of
      (_ : kbTxt : _) -> case reads kbTxt of
        [(kb, "")] -> Just (kb * 1024)
        _          -> Nothing
      _ -> Nothing
    [] -> Nothing
  where
    isPrefixOfStr p s = take (length p) s == p

-- | IO: the current RSS of this process in bytes, read from
-- @\/proc\/self\/status@. Linux only (CPP); 'Nothing' on other platforms
-- or when the file cannot be read/parsed.
readCurrentRssBytes :: IO (Maybe Integer)
#ifdef FMS_LINUX
readCurrentRssBytes = do
  r <- try (BS.readFile "/proc/self/status") :: IO (Either SomeException BS.ByteString)
  case r of
    Left _     -> pure Nothing
    Right body -> pure (parseProcStatusRss (BSC.unpack body))
#else
readCurrentRssBytes = pure Nothing
#endif

-- | Pure: trim a ring buffer to at most @cap@ entries, keeping the NEWEST
-- (the tail of the list, samples are appended oldest-first). Exported
-- for tests.
trimRing :: Int -> [a] -> [a]
trimRing cap xs = drop (max 0 (length xs - cap)) xs

-- | Fork the memory sampler (docs/todo.org section 15): every
-- 'configMemorySampleSecs' seconds (0 disables) read the RSS, append it
-- to the ring buffer under 'memSamplesKey' (trimmed to 'memRingCap'), and
-- log a warn line when RSS crosses 'configMemoryWarnMb' (at most once per
-- crossing). Called once from @runServer@; the thread is a daemon. The
-- loop body is wrapped in 'try' so it can never kill the server.
startMemorySampler :: AppM ()
startMemorySampler = do
  cfg <- asks envConfig
  env <- ask
  -- 0 disables sampling entirely (no thread forked)
  when (configMemorySampleSecs cfg > 0) $ do
    _ <- liftIO (forkIO (samplerLoop env))
    pure ()

samplerLoop :: AppEnv -> IO ()
samplerLoop env = forever $ do
  let secs = configMemorySampleSecs (envConfig env)
  threadDelay (secs * 1000000)
  _ <- try (runApp env sampleOnce) :: IO (Either SomeException (Either AppError ()))
  pure ()

-- | One sample: read RSS, append to the ring buffer, warn on crossing.
sampleOnce :: AppM ()
sampleOnce = do
  cfg <- asks envConfig
  mRss <- liftIO readCurrentRssBytes
  case mRss of
    Nothing -> pure ()
    Just rss -> do
      now <- liftIO getCurrentTime
      tv <- ensureMemTVar
      warnNow <- liftIO $ atomically $ do
        (samples, wasWarn) <- readTVar tv
        let samples' = trimRing memRingCap (samples ++ [(now, rss)])
            warnBytes = fromIntegral (configMemoryWarnMb cfg) * 1024 * 1024 :: Integer
            warnNow' = rss > warnBytes && not wasWarn
            wasWarn' = wasWarn || (rss > warnBytes)
        writeTVar tv (samples', wasWarn')
        pure warnNow'
      when warnNow $
        logWarn ("memory: RSS " <> T.pack (show rss) <> " bytes exceeds "
                 <> T.pack (show (configMemoryWarnMb cfg)) <> " MB")

ensureMemTVar :: AppM (TVar ([(UTCTime, Integer)], Bool))
ensureMemTVar = do
  m <- getState memSamplesKey
  case m of
    Just tv -> pure tv
    Nothing -> do
      tv <- liftIO (newTVarIO ([], False))
      putState memSamplesKey tv
      pure tv

-- | Read the current samples + warn flag (used by the /System/Memory
-- endpoint and tests).
readMemSamples :: AppM ([(UTCTime, Integer)], Bool)
readMemSamples = do
  tv <- ensureMemTVar
  liftIO (readTVarIO tv)

-- | Helper for endpoints: current RSS (0 when unavailable).
currentRssOrZero :: AppM Integer
currentRssOrZero = do
  m <- liftIO readCurrentRssBytes
  pure (fromMaybe 0 m)
