module FMS.Core.Query
  ( queryParams
  , searchItems
  , filterByFolder
  , filterByType
  , filterBySeries
  , filterByIncludeTypes
  , itemYear
  , filterByYears
  , showsFromItems
  , sortEpisodes
  , sortItems
  , paginate
  , applyItemQuery
  ) where

import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.List (sortBy)
import Data.Maybe (catMaybes, fromMaybe)
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

-- | Keep only items scanned from the given media folder (milestone 2c).
-- 'Nothing' (no @libraryId@ param) keeps everything.
filterByFolder :: Maybe Text -> [MediaItem] -> [MediaItem]
filterByFolder mFolder = case mFolder of
  Nothing  -> id
  Just fid -> filter (\i -> itemFolder i == fid)

-- | Keep only items of the given type (milestone 4d:
-- @?type=Movie@ / @?type=Episode@ / @?type=Audio@).
filterByType :: Maybe Text -> [MediaItem] -> [MediaItem]
filterByType mType = case mType of
  Nothing -> id
  Just t  -> filter (\i -> itemType i == t)

-- | Keep only episodes of the given series (milestone 4d: @?series=...@).
filterBySeries :: Maybe Text -> [MediaItem] -> [MediaItem]
filterBySeries mSeries = case mSeries of
  Nothing -> id
  Just s  -> filter (\i -> itemSeries i == Just s)

-- | Milestone 4e: keep only items whose 'itemType' appears in the
-- comma-separated @includeItemTypes@ list (case-insensitive, whitespace
-- trimmed). Missing or empty param -> identity.
filterByIncludeTypes :: Maybe Text -> [MediaItem] -> [MediaItem]
filterByIncludeTypes mParam = case mParam of
  Nothing -> id
  Just raw
    | null wanted -> id
    | otherwise   -> filter (\i -> T.toCaseFold (itemType i) `elem` wanted)
    where
      wanted = [ T.toCaseFold (T.strip t)
               | t <- T.splitOn "," raw, not (T.null (T.strip t)) ]

-- | The item's release year, if the item carries one. Milestone 4e:
-- 'MediaItem' has no year field (checked in "FMS.Core.Types.MediaItem"),
-- so this is always 'Nothing' for now and a non-empty @years@ list
-- matches no item (Jellyfin semantics: an item matches only when its
-- year is present and listed).
itemYear :: MediaItem -> Maybe Int
itemYear _ = Nothing

-- | Milestone 4e: keep only items whose year is in the comma-separated
-- @years@ list of ints. Invalid tokens are dropped; when no valid int
-- remains the param acts as absent (identity).
filterByYears :: Maybe Text -> [MediaItem] -> [MediaItem]
filterByYears mParam = case mParam of
  Nothing -> id
  Just raw
    | null years -> id
    | otherwise  -> filter (\i -> maybe False (`elem` years) (itemYear i))
    where
      years = catMaybes [ readMaybe (T.unpack (T.strip t))
                        | t <- T.splitOn "," raw, not (T.null (T.strip t)) ]

-- | Pure (milestone 4d): the series present in the library as
-- @(name, episodeCount)@, sorted by name. Only @Episode@ items with a
-- series name count.
showsFromItems :: [MediaItem] -> [(Text, Int)]
showsFromItems items =
  Map.toAscList (Map.fromListWith (+)
    [ (s, 1)
    | i <- items
    , itemType i == "Episode"
    , Just s <- [itemSeries i]
    ])

-- | Pure (milestone 4d): order episodes by (season, episode, title);
-- episodes without numbers sort before numbered ones (Nothing < Just).
sortEpisodes :: [MediaItem] -> [MediaItem]
sortEpisodes = sortBy (comparing (\i -> (itemSeason i, itemEpisode i, itemTitle i)))

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
-- list of items: search, then the @libraryId@ folder filter (milestone
-- 2c), then the @type@ and @series@ filters (milestone 4d), then the
-- @includeItemTypes@ and @years@ filters (milestone 4e), then sort, then
-- paginate. @searchTerm@ is an alias of @search@ (@search@ wins when both
-- are present). @recursive@ is accepted and ignored (the library is flat)
-- and @filters@ is parsed but its IsPlayed/IsFavorite/IsResumable values
-- are deferred -- unknown params are ignored anyway. Returns the total
-- record count taken /before/ pagination together with the page. Integer
-- params (@startIndex@, @limit@) are parsed with 'readMaybe'; invalid
-- values are ignored and the defaults apply.
applyItemQuery :: [(Text, Text)] -> [MediaItem] -> (Int, [MediaItem])
applyItemQuery params items =
  let get name = lookup name params
      searchTerm = case get "search" of
        Just s  -> Just s
        Nothing -> get "searchTerm"          -- milestone 4e alias
      searched = maybe items (`searchItems` items) searchTerm
      inFolder = filterByFolder (get "libraryId") searched
      typed    = filterByType (get "type") inFolder
      series   = filterBySeries (get "series") typed
      incTypes = filterByIncludeTypes (get "includeItemTypes") series
      years    = filterByYears (get "years") incTypes
      sorted   = sortItems (get "sortBy") (get "sortOrder") years
      total    = length sorted
      start    = get "startIndex" >>= readMaybe . T.unpack
      limit    = get "limit" >>= readMaybe . T.unpack
  in (total, paginate start limit sorted)
