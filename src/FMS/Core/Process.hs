module FMS.Core.Process
  ( runProcessBS
  , runProcessBSTimeout
  , runProcessStreamBS
  , runProcessStreamBSTimeout
  , tryAcquireSlot
  , ffmpegTimeoutArgs
  , ffmpegSlotsKey
  , showBS
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Concurrent.QSemN (QSemN, waitQSemN)
import Control.Exception (SomeException, bracket, evaluate, try)
import Control.Monad (unless, void, when)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (isPrefixOf)
import Data.Maybe (isJust)
import Data.Text (Text)
import System.Exit (ExitCode)
import System.IO (Handle, hClose)
import System.Process
  ( CreateProcess(..)
  , ProcessHandle
  , StdStream(..)
  , createProcess
  , proc
  , terminateProcess
  , waitForProcess
  )
import System.Timeout (timeout)

-- | Process helpers that never decode bytes through the locale encoding.
--
-- The milestone-6a bug was a 500 on @POST /items/<unprobeable>/PlaybackInfo@:
-- a module init set the process-global locale to latin1 and a log string
-- contained an em-dash (U+2014), so 'System.IO.hPutChar' threw while
-- printing. The fix is to never route subprocess output (or any raw byte
-- string) through 'String': everything below reads pipes as raw bytes with
-- 'BS.hGetContents' and renders them with 'showBS', which maps one byte to
-- one char and can never throw.

-- | Registry key of the global ffmpeg slot semaphore ('QSemN') created in
-- @runServer@ (milestone 15): the maximum number of concurrent ffmpeg
-- jobs (transcodes + HLS/DASH packaging) is 'configMaxTranscodes'.
ffmpegSlotsKey :: Text
ffmpegSlotsKey = "ffmpeg.slots"

-- | Run @cmd args@ to completion, capturing stdout and stderr as raw
-- bytes. Both pipes are drained concurrently (stderr on a helper thread)
-- so a chatty child can never deadlock on a full pipe, then the process is
-- reaped with 'waitForProcess'. The pipes are always closed and the
-- process reaped, even if reading throws. No timeout (see
-- 'runProcessBSTimeout').
runProcessBS
  :: FilePath
  -> [String]
  -> IO (ExitCode, LBS.ByteString, LBS.ByteString)
runProcessBS = runProcessBSTimeout 0

-- | Like 'runProcessBS' but terminates the child (SIGTERM via
-- 'terminateProcess', which ffmpeg handles by exiting) after
-- @timeoutSec@ seconds of wall-clock time (milestone 15; @<= 0@ disables
-- the guard). The killer thread is disarmed when the body finishes, and
-- 'terminateProcess' on an already-reaped handle is caught, so a late
-- fire is harmless.
runProcessBSTimeout
  :: Int
  -> FilePath
  -> [String]
  -> IO (ExitCode, LBS.ByteString, LBS.ByteString)
runProcessBSTimeout timeoutSec cmd args =
  withPipesTimeout timeoutSec cmd args $ \ph out err -> do
    errBox <- newEmptyMVar
    _ <- forkIO (readToEOF err >>= putMVar errBox)
    outBs <- BS.hGetContents out
    _ <- evaluate (BS.length outBs)
    errBs <- readMVar errBox
    ec <- waitForProcess ph
    pure (ec, LBS.fromStrict outBs, LBS.fromStrict errBs)

-- | Run @cmd args@ and stream its stdout to @sink@ in chunks of 64 KiB
-- while stderr is drained concurrently and returned as raw bytes together
-- with the exit code. Used where stdout must go straight to the client
-- (transcode / subtitle conversion). If @sink@ throws (client
-- disconnected), the pipes are still closed and the process reaped.
-- No timeout (see 'runProcessStreamBSTimeout').
runProcessStreamBS
  :: FilePath
  -> [String]
  -> (BS.ByteString -> IO ())
  -> IO (ExitCode, LBS.ByteString)
runProcessStreamBS cmd args sink = runProcessStreamBSTimeout 0 cmd args sink

-- | Like 'runProcessStreamBS' but kills the child after @timeoutSec@
-- seconds of wall-clock time (milestone 15; @<= 0@ disables the guard),
-- so a hung ffmpeg can never pin a transcode slot forever.
runProcessStreamBSTimeout
  :: Int
  -> FilePath
  -> [String]
  -> (BS.ByteString -> IO ())
  -> IO (ExitCode, LBS.ByteString)
runProcessStreamBSTimeout timeoutSec cmd args sink =
  withPipesTimeout timeoutSec cmd args $ \ph out err -> do
    errBox <- newEmptyMVar
    _ <- forkIO (readToEOF err >>= putMVar errBox)
    let go = do
          chunk <- BS.hGet out processChunkSize
          if BS.null chunk
            then pure ()  -- EOF: the child finished (or died)
            else do
              sink chunk
              go
    go
    errBs <- readMVar errBox
    ec <- waitForProcess ph
    pure (ec, LBS.fromStrict errBs)

-- | Try to acquire one unit of @sem@, waiting at most @maxWaitSec@
-- seconds; returns 'False' on timeout. Implemented as
-- 'System.Timeout.timeout' around 'waitQSemN' (base's 'QSemN' has no
-- non-blocking try-wait); @maxWaitSec <= 0@ still gets a single 250 ms
-- poll, which returns immediately when a unit is free because
-- 'waitQSemN' then completes synchronously. Milestone 15: used to take an
-- ffmpeg slot with a short grace period before answering 503. The caller
-- must 'signalQSemN' the semaphore when done.
tryAcquireSlot :: QSemN -> Int -> IO Bool
tryAcquireSlot sem maxWaitSec =
  isJust <$> timeout waitUs (waitQSemN sem 1)
  where
    waitUs = (if maxWaitSec <= 0 then 250 else maxWaitSec * 1000) * 1000

-- | ffmpeg input-timeout arguments (milestone 15). The killer thread in
-- 'runProcessBSTimeout' bounds the whole job; this additionally sets
-- ffmpeg's own @-rw_timeout@ (microseconds) for NETWORK inputs only,
-- where a hung connection is the realistic failure - for local files the
-- option is pointless, so it is omitted. @timeoutSec <= 0@ -> @[]@.
-- Must be placed before @-i@ in the argument list. Pure.
ffmpegTimeoutArgs :: Int -> FilePath -> [String]
ffmpegTimeoutArgs timeoutSec input
  | timeoutSec <= 0 = []
  | isUrlInput input = ["-rw_timeout", show (timeoutSec * 1000000)]
  | otherwise        = []
  where
    isUrlInput p = any (`isPrefixOf` p)
      [ "http://", "https://", "rtsp://", "rtmpt://", "rtmp://"
      , "rtp://", "tcp://", "udp://" ]

-- | Render raw bytes as a 'String' with the per-byte latin1 mapping
-- (byte @0xE2@ -> char @U+00E2@). Unlike locale decoding this never
-- throws, so it is safe for arbitrary subprocess output.
showBS :: BS.ByteString -> String
showBS = BSC.unpack

-- | Create the process with both pipes, run the body, and always close
-- the handles and reap the process afterwards (also on exceptions).
-- Milestone 15: when @timeoutSec > 0@ a killer thread terminates the
-- child after that many seconds (disarmed by the 'done' flag on normal
-- exit).
withPipesTimeout
  :: Int
  -> FilePath
  -> [String]
  -> (ProcessHandle -> Handle -> Handle -> IO a)
  -> IO a
withPipesTimeout timeoutSec cmd args body =
  bracket start stop $ \(ph, out, err, _done) -> body ph out err
  where
    start = do
      r <- createProcess (proc cmd args)
            { std_out = CreatePipe, std_err = CreatePipe }
      case r of
        (_, Just out, Just err, ph) -> do
          done <- newIORef False
          _ <- forkIO $ when (timeoutSec > 0) $ do
            threadDelay (timeoutSec * 1000000)
            finished <- readIORef done
            unless finished $
              void (try (terminateProcess ph) :: IO (Either SomeException ()))
          pure (ph, out, err, done)
        _ -> fail ("could not create stdout/stderr pipes for " ++ cmd)

    stop :: (ProcessHandle, Handle, Handle, IORef Bool) -> IO ()
    stop (ph, out, err, done) = do
      writeIORef done True
      _ <- try (hClose out) :: IO (Either SomeException ())
      _ <- try (hClose err) :: IO (Either SomeException ())
      _ <- try (waitForProcess ph) :: IO (Either SomeException ExitCode)
      pure ()

-- | Read a pipe to EOF as raw bytes. If the handle is closed
-- concurrently (cleanup after an exception) the partial read is
-- discarded and 'BS.empty' is returned, so the helper thread never dies
-- with an unhandled exception.
readToEOF :: Handle -> IO BS.ByteString
readToEOF h = do
  r <- try (do
         bs <- BS.hGetContents h
         _ <- evaluate (BS.length bs)
         pure bs) :: IO (Either SomeException BS.ByteString)
  pure (either (const BS.empty) id r)

-- | Streaming chunk size: 64 KiB.
processChunkSize :: Int
processChunkSize = 64 * 1024
