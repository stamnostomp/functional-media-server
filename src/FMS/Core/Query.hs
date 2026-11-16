module FMS.Core.Query
  ( queryParams
  , searchItems
  , filterByFolder
  , filterByType
  , filterBySeries
  , filterByIncludeTypes
  , itemYear
  , filterByYears
  , typeFilterIncludesBoxSet
  , applyUserDataFilters
  , applyMetadataFilters
  , projectFields
  , attachUserData
  , showsFromItems
  , sortEpisodes
  , specialFeaturesOf
  , sortItems
  , paginate
  , applyItemQuery
  ) where

import Data.Aeson (ToJSON (..), Value (..))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString (ByteString)
import qualified Data.Map.Strict as Map
import Data.List (sortBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (urlDecode)
import System.FilePath (takeDirectory)
import Text.Read (readMaybe)

import FMS.Core.Types.MediaItem (MediaItem(..))
import FMS.Modules.Metadata.Types (MediaMetadata (..))
import FMS.Modules.UserData.Types (UserData (..), emptyUserData)

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

-- | Pure (milestone 5e): apply the @filters=@ tokens (comma-separated,
-- case-insensitive, whitespace trimmed) against a per-item 'UserData'
-- lookup. All tokens are ANDed; unknown tokens are ignored; an empty
-- token list is the identity. Semantics: IsPlayed -> 'udPlayed',
-- Unplayed -> not 'udPlayed', IsFavorite/IsLiked -> 'udFavorite',
-- IsResumable -> a 'udPlaybackPosition' > 0 on an unplayed item. A
-- missing 'UserData' record counts as not-played / not-favorite / not
-- resumable.
applyUserDataFilters
  :: [Text]                     -- ^ filter tokens
  -> (Text -> Maybe UserData)   -- ^ itemId -> 'UserData' lookup
  -> [MediaItem]
  -> [MediaItem]
applyUserDataFilters tokens lookupItems = case tokens of
  [] -> id
  ts -> filter (\i -> all (matches (lookupItems (itemId i))) ts)
  where
    matches mUd tok = case T.toCaseFold (T.strip tok) of
      "isplayed"    -> maybe False udPlayed mUd
      "unplayed"    -> not (maybe False udPlayed mUd)
      "isfavorite"  -> maybe False udFavorite mUd
      "isliked"     -> maybe False udFavorite mUd
      "isresumable" -> case mUd of
        Just u  -> maybe False (> 0) (udPlaybackPosition u) && not (udPlayed u)
        Nothing -> False
      _             -> True   -- unknown token ignored

-- | Pure (milestone 5e): keep only the requested object keys of an item
-- JSON value, compared case-insensitively against the object's keys;
-- the @id@ key is always kept. An empty fields list is the identity and
-- a non-object value is returned unchanged.
projectFields :: [Text] -> Value -> Value
projectFields fields v = case v of
  Object o
    | null wanted -> v
    | otherwise   -> Object (KeyMap.filterWithKey (\k _ -> keep k) o)
    where
      wanted = [ T.toCaseFold (T.strip f) | f <- fields, not (T.null (T.strip f)) ]
      keep k = k == Key.fromText "id" || T.toCaseFold (Key.toText k) `elem` wanted
  _ -> v

-- | Pure (milestone 7): does the effective type filter (@type@ or
-- @includeItemTypes@) include BoxSet, case-insensitively? When it does,
-- @\/items@ appends the synthesized BoxSet pseudo-items (collections).
typeFilterIncludesBoxSet :: [(Text, Text)] -> Bool
typeFilterIncludesBoxSet params =
  maybe False (\t -> T.toCaseFold (T.strip t) == "boxset") (lookup "type" params)
  || maybe False (\raw -> "boxset" `elem`
                    [ T.toCaseFold (T.strip x) | x <- T.splitOn "," raw ])
               (lookup "includeItemTypes" params)

-- | Pure (milestone 4h): apply the metadata-backed query params @genres@,
-- @studios@ and @persons@ (comma-separated, case-insensitive) using a
-- per-itemId 'MediaMetadata' lookup. An item matches a param when ANY of
-- the param tokens equals (case-insensitively) ANY entry of the
-- corresponding metadata list; different params are ANDed together;
-- absent/empty params are the identity; an item with NO stored metadata
-- matches only absent/empty params.
applyMetadataFilters
  :: [(Text, Text)]                 -- ^ query params
  -> (Text -> Maybe MediaMetadata)  -- ^ itemId -> metadata lookup
  -> [MediaItem]
  -> [MediaItem]
applyMetadataFilters params lookupMeta =
  foldl applyParam id
        [ ("genres", metaGenres), ("studios", metaStudios), ("persons", metaPersons) ]
  where
    applyParam acc (key, getField) = case tokensOf (lookup key params) of
      []   -> acc
      toks -> \xs -> filter (\i -> case lookupMeta (itemId i) of
                              Just m  -> any (`elem` toks) (map T.toLower (getField m))
                              Nothing -> False)
                           (acc xs)
    tokensOf Nothing     = []
    tokensOf (Just raw)  =
      [ T.toLower (T.strip t) | t <- T.splitOn "," raw, not (T.null (T.strip t)) ]

-- | Pure (milestone 5e): attach a @UserData@ key to an item's JSON object
-- when a lookup is given (the looked-up record, or 'emptyUserData' when
-- the item has none). Without a lookup, or on a non-object value, the
-- value is returned unchanged.
attachUserData
  :: Maybe (Text -> Maybe UserData)  -- ^ itemId -> 'UserData' lookup
  -> MediaItem
  -> Value
  -> Value
attachUserData mLookup item v = case (mLookup, v) of
  (Just lk, Object o) ->
    Object (KeyMap.insert (Key.fromText "UserData") (toJSON (fromMaybe emptyUserData (lk (itemId item)))) o)
  _ -> v

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

-- | Pure (milestone 4g): the special features (Extra items) belonging to
-- @item@ — the visible Extra items whose file lives in the same directory
-- as the item's file or one level below it, sorted by title.
specialFeaturesOf :: MediaItem -> [MediaItem] -> [MediaItem]
specialFeaturesOf item allItems =
  sortBy (comparing itemTitle)
    [ i | i <- allItems
        , itemType i == "Extra"
        , let d    = takeDirectory (itemPath i)
              home = takeDirectory (itemPath item)
        , d == home || takeDirectory d == home
        ]

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
