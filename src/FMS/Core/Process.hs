module FMS.Core.Process
  ( runProcessBS
  , runProcessStreamBS
  , showBS
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar)
import Control.Exception (SomeException, bracket, evaluate, try)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import System.Exit (ExitCode)
import System.IO (Handle, hClose)
import System.Process
  ( CreateProcess(..)
  , ProcessHandle
  , StdStream(..)
  , createProcess
  , proc
  , waitForProcess
  )

-- | Process helpers that never decode bytes through the locale encoding.
--
-- The milestone-6a bug was a 500 on @POST /items/<unprobeable>/PlaybackInfo@:
-- a module init set the process-global locale to latin1 and a log string
-- contained an em-dash (U+2014), so 'System.IO.hPutChar' threw while
-- printing. The fix is to never route subprocess output (or any raw byte
-- string) through 'String': everything below reads pipes as raw bytes with
-- 'BS.hGetContents' and renders them with 'showBS', which maps one byte to
-- one char and can never throw.

-- | Run @cmd args@ to completion, capturing stdout and stderr as raw
-- bytes. Both pipes are drained concurrently (stderr on a helper thread)
-- so a chatty child can never deadlock on a full pipe, then the process is
-- reaped with 'waitForProcess'. The pipes are always closed and the
-- process reaped, even if reading throws.
runProcessBS
  :: FilePath
  -> [String]
  -> IO (ExitCode, LBS.ByteString, LBS.ByteString)
runProcessBS cmd args =
  withPipes cmd args $ \ph out err -> do
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
runProcessStreamBS
  :: FilePath
  -> [String]
  -> (BS.ByteString -> IO ())
  -> IO (ExitCode, LBS.ByteString)
runProcessStreamBS cmd args sink =
  withPipes cmd args $ \ph out err -> do
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

-- | Render raw bytes as a 'String' with the per-byte latin1 mapping
-- (byte @0xE2@ -> char @U+00E2@). Unlike locale decoding this never
-- throws, so it is safe for arbitrary subprocess output.
showBS :: BS.ByteString -> String
showBS = BSC.unpack

-- | Create the process with both pipes, run the body, and always close
-- the handles and reap the process afterwards (also on exceptions).
withPipes
  :: FilePath
  -> [String]
  -> (ProcessHandle -> Handle -> Handle -> IO a)
  -> IO a
withPipes cmd args body =
  bracket start stop $ \(ph, out, err) -> body ph out err
  where
    start = do
      r <- createProcess (proc cmd args)
            { std_out = CreatePipe, std_err = CreatePipe }
      case r of
        (_, Just out, Just err, ph) -> pure (ph, out, err)
        _ -> fail ("could not create stdout/stderr pipes for " ++ cmd)

    stop :: (ProcessHandle, Handle, Handle) -> IO ()
    stop (ph, out, err) = do
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
