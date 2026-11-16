module FMS.Core.Query
  ( queryParams
  , searchItems
  , sortItems
  , paginate
  , applyItemQuery
  ) where

import Data.ByteString (ByteString)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (urlDecode)
import Text.Read (readMaybe)

import FMS.Core.Types.MediaItem (MediaItem(..))

-- | Decode a WAI query string (@[(ByteString, Maybe ByteString)]@, see
-- 'Network.Wai.queryString') into @[(Text, Text)]@: percent-decode with
-- @+@ read as space (query-string convention, hence 'urlDecode' @True@),
-- drop parameters that have no value (@Nothing@), then decode UTF-8.
queryParams :: [(ByteString, Maybe ByteString)] -> [(Text, Text)]
queryParams qs = [ (dec k, dec v) | (k, Just v) <- qs ]
  where
    dec = TE.decodeUtf8 . urlDecode True

-- | Case-insensitive substring match on 'itemTitle'. An empty search term
-- is a substring of every title, so @?search=@ behaves like no filter.
searchItems :: Text -> [MediaItem] -> [MediaItem]
searchItems term =
  filter (\i -> T.toLower term `T.isInfixOf` T.toLower (itemTitle i))

-- | Sort by @sortBy@ ∈ {title, size, addedAt} — unknown or missing falls
-- back to title — with direction @sortOrder@ ∈ {asc, desc} (default asc).
-- 'sortBy' is stable: items with equal keys keep their relative order.
sortItems :: Maybe Text -> Maybe Text -> [MediaItem] -> [MediaItem]
sortItems mSortBy mSortOrder items = sortBy cmp items
  where
    base = case mSortBy of
      Just "size"    -> comparing itemSize
      Just "addedAt" -> comparing itemAddedAt
      _              -> comparing itemTitle  -- "title", unknown, Nothing
    cmp = case mSortOrder of
      Just "desc" -> \x y -> base y x
      _           -> base  -- "asc", anything else, Nothing

-- | Pagination window: @startIndex@ (default 0, clamped to >= 0) and
-- @limit@ (default: all remaining items). A negative limit yields an
-- empty page ('take' of a negative count).
paginate :: Maybe Int -> Maybe Int -> [a] -> [a]
paginate mStart mLimit xs =
  let start = max 0 (fromMaybe 0 mStart)
  in case mLimit of
       Nothing -> drop start xs
       Just l  -> take l (drop start xs)

-- | Apply the Jellyfin-shaped items query (docs/todo.org section 4) to a
-- list of items: search first, then sort, then paginate. Returns the
-- total record count taken /before/ pagination together with the page.
-- Integer params (@startIndex@, @limit@) are parsed with 'readMaybe';
-- invalid values are ignored and the defaults apply.
applyItemQuery :: [(Text, Text)] -> [MediaItem] -> (Int, [MediaItem])
applyItemQuery params items =
  let get name = lookup name params
      searched = maybe items (`searchItems` items) (get "search")
      sorted   = sortItems (get "sortBy") (get "sortOrder") searched
      total    = length sorted
      start    = get "startIndex" >>= readMaybe . T.unpack
      limit    = get "limit" >>= readMaybe . T.unpack
  in (total, paginate start limit sorted)
