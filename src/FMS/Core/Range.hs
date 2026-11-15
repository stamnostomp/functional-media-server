module FMS.Core.Range
  ( ByteRange(..)
  , parseRangeHeader
  ) where

import Data.ByteString (ByteString)
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Text.Read (readMaybe)

-- | A single byte range. @rangeEnd = Nothing@ means "to end of file".
data ByteRange = ByteRange
  { rangeStart :: !Int64
  , rangeEnd   :: !(Maybe Int64)
  } deriving (Show, Eq)

-- | Parse a single-range @Range@ header value: @bytes=start-end@ or
-- @bytes=start-@. Anything unparsable (including @bytes=-suffix@ forms and
-- ranges whose end is smaller than their start, e.g. @bytes=5-2@) yields
-- 'Nothing', which callers treat as "no range requested" (docs/modules.org:
-- malformed headers mean "no range").
parseRangeHeader :: ByteString -> Maybe ByteRange
parseRangeHeader raw = do
  body <- T.stripPrefix "bytes=" (T.strip (TE.decodeUtf8 raw))
  let (startTxt, rest) = T.break (== '-') body
  start <- readMaybe (T.unpack startTxt) :: Maybe Int64
  case T.uncons rest of
    Just ('-', endTxt)
      | T.null endTxt -> pure (ByteRange start Nothing)
      | otherwise -> do
          end <- readMaybe (T.unpack (T.strip endTxt)) :: Maybe Int64
          if end < start
            then Nothing  -- e.g. "bytes=5-2": invalid, treated as no range
            else pure (ByteRange start (Just end))
    _ -> Nothing
