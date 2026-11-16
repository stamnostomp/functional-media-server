module FMS.Modules.Stream
  ( streamModule
  , resolveRange
  ) where

import Control.Exception (bracket)
import Control.Monad (when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as BS
import Data.ByteString.Builder (Builder, byteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Types (Header, hRange, status200, status206)
import Network.Wai (Request, Response, queryString, requestHeaders, responseStream)
import System.Directory (getFileSize)
import System.IO (IOMode(ReadMode), SeekMode(AbsoluteSeek), hClose, hSeek, openBinaryFile)

import FMS.Core.Module (Module(..), Segment(..), get)
import FMS.Core.Monad (AppError(..), AppM)
import FMS.Core.Query (queryParams, versionParam, versionPathFor)
import FMS.Core.Range (ByteRange(..), parseRangeHeader)
import FMS.Core.Types.MediaItem (MediaItem(..))
import FMS.Modules.Auth.Monad (getItemWithAccess)
import FMS.Modules.Stats (MonadStats(..))

-- | The stream module (docs/roadmap.org milestone 3, docs/todo.org
-- section 6): @GET /stream/:id@ serves the file bytes of a library item,
-- honouring a single-range @Range@ request header (206 Partial Content).
-- It only needs the 'MonadLibrary' capability — the core never learns about
-- it (docs/modules.org).
streamModule :: Module
streamModule = Module
  { moduleName   = "stream"
  , moduleInit   = pure ()
  , moduleRoutes = [get [Lit "stream", Capture "id"] streamHandler]
  }

-- | @GET /stream/:id@ — the whole file (200), or one byte range (206).
-- A missing item is 404; a range that cannot be satisfied is 400.
--
-- The bytes are streamed by hand with 'responseStream' rather than served
-- with @responseFile@: warp applies its own Range handling to every
-- file response (a malformed range gets warp's 416 and a non-bytes range
-- unit stalls the connection), which would override our documented
-- semantics below. With 'responseStream' warp adds nothing and does no
-- range processing, so no duplicate or foreign @Accept-Ranges@ /
-- @Content-Range@ headers can appear either.
streamHandler :: [(Text, Text)] -> Request -> AppM Response
streamHandler captures req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just i  -> do
      mItem <- getItemWithAccess req i
      case mItem of
        Nothing -> throwError (NotFound ("no library item with id " <> i))
        Just item -> do
          -- milestone 4j: ?version=N picks a file from the item's version
          -- list (default 0 = primary); invalid -> 400
          path <- case versionParam (queryParams (queryString req)) of
            Left msg -> throwError (BadRequest msg)
            Right n  -> case versionPathFor item n of
              Left msg   -> throwError (BadRequest msg)
              Right path -> pure path
          size <- liftIO (getFileSize path)
          let baseHeaders =
                [ ("Content-Type", encodeUtf8 (itemMime item))
                , ("Accept-Ranges", "bytes")
                ]
          -- docs/modules.org: a missing or malformed Range header means
          -- "no range requested" -> serve the full file with 200.
          -- Milestone 5a: a full 200 response, or a range starting at
          -- byte 0, counts as a play -> recordPlay (stats module).
          case lookup hRange (requestHeaders req) >>= parseRangeHeader of
            Nothing -> do
              recordPlay (itemId item)
              pure (responseStream status200
                      (baseHeaders ++ [contentLength size])
                      (streamFileRange path 0 size))
            Just br ->
              case resolveRange size br of
                Left msg -> throwError (BadRequest msg)
                Right (start, end) -> do
                  when (start == 0) (recordPlay (itemId item))
                  pure (responseStream status206
                          ( baseHeaders
                            ++ [ contentLength (end - start + 1)
                               , ("Content-Range", encodeUtf8 (contentRange start end size))
                               ]
                          )
                          (streamFileRange path start (end - start + 1)))

-- | A @Content-Length@ header pair for a byte count.
contentLength :: Integer -> Header
contentLength n = ("Content-Length", encodeUtf8 (T.pack (show n)))

-- | The 'Network.Wai.StreamingBody' used with 'responseStream': write
-- exactly @len@ bytes of the file at @path@, starting at byte offset
-- @start@, to the WAI sink in chunks of 64 KiB. The file is opened in
-- binary mode and always closed via 'bracket'; the second WAI argument
-- (a flush) is run once after the last chunk.
streamFileRange
  :: FilePath -> Integer -> Integer
  -> (Builder -> IO ()) -> IO () -> IO ()
streamFileRange path start len send flush =
  bracket (openBinaryFile path ReadMode) hClose $ \h -> do
    hSeek h AbsoluteSeek start
    let go remaining
          | remaining <= 0 = pure ()
          | otherwise = do
              chunk <- BS.hGet h (fromInteger (min remaining chunkSize))
              if BS.null chunk
                then pure ()  -- EOF earlier than expected; stop cleanly
                else do
                  send (byteString chunk)
                  go (remaining - toInteger (BS.length chunk))
    go len
    flush

-- | Streaming chunk size: 64 KiB.
chunkSize :: Integer
chunkSize = 64 * 1024

-- | The @Content-Range@ header value: @bytes start-end/size@.
contentRange :: Integer -> Integer -> Integer -> Text
contentRange start end size =
  T.pack ("bytes " ++ show start ++ "-" ++ show end ++ "/" ++ show size)

-- | Resolve a parsed 'ByteRange' against a file size into a concrete,
-- inclusive @(start, end)@ pair. Pure so the edge cases are unit-testable:
-- a negative start, a start at or beyond EOF, an end beyond EOF, or an end
-- before the start are all 'Left' with a message the handler maps to 400.
-- @end = Nothing@ means "to end of file", clamped to @size - 1@.
resolveRange :: Integer -> ByteRange -> Either Text (Integer, Integer)
resolveRange size br =
  let start = toInteger (rangeStart br)
      endM  = toInteger <$> rangeEnd br
      showI = T.pack . show
  in if start < 0
       then Left ("range start is negative: " <> showI start)
       else if start >= size
         then Left ("range start " <> showI start
                    <> " is beyond file size " <> showI size)
         else case endM of
           Nothing -> Right (start, size - 1)
           Just end
             | end < start ->
                 Left ("range end " <> showI end
                       <> " is before range start " <> showI start)
             | end >= size ->
                 Left ("range end " <> showI end
                       <> " is beyond file size " <> showI size)
             | otherwise -> Right (start, end)
