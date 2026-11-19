module FMS.Core.Query
  ( queryParams
  , normalizeQueryParams
  , searchItems
  , searchItemsMeta
  , notInAnyCollection
  , imageTagsValue
  , backdropImageTagsValue
  , attachImageTags
  , attachTags
  , withDisplayTitle
  , withDisplayTitleLower
  , withLegacyType
  , displayTitleForItem
  , withLegacyItem
  , imageBlurHashesValue
  , attachImageBlurHashes
  , filterByFolder
  , filterByParent
  , filterByType
  , filterBySeries
  , filterByIncludeTypes
  , itemYear
  , filterByYears
  , typeFilterIncludesBoxSet
  , applyUserDataFilters
  , sortByUserDataField
  , isUserDataSortKey
  , applyMetadataFilters
  , projectFields
  , attachUserData
  , showsFromItems
  , similarItems
  , versionParam
  , versionPathFor
  , sortEpisodes
  , specialFeaturesOf
  , sortItems
  , paginate
  , clampNonNegative
  , clampStartIndex
  , clampLimit
  , parseBoundedInt
  , applyItemQuery
  , viewsFromFolders
  , nextUpEpisodes
  , latestItems
  ) where

import Data.Aeson (ToJSON (..), Value (..), object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as Map
import Data.List (sortBy)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (urlDecode)
import System.FilePath (takeDirectory)
import Text.Read (readMaybe)

import FMS.Core.Types.MediaFolder (MediaFolder (..))
import FMS.Core.Types.MediaItem (MediaItem(..))
import FMS.Modules.Metadata.Types (MediaMetadata (..))
import FMS.Modules.UserData.Types (UserData (..), emptyUserData)

-- | Decode a WAI query string (@[(ByteString, Maybe ByteString)]@, see
-- 'Network.Wai.queryString') into @[(Text, Text)]@: percent-decode with
-- @+@ read as space (query-string convention, hence 'urlDecode' @True@),
-- drop parameters that have no value (@Nothing@), then decode UTF-8.
-- Milestone 17a: parameter NAMES are case-folded to lowercase (values
-- untouched) via 'normalizeQueryParams', so jellyfin-web's PascalCase
-- @?Filters=@ / @?IncludeItemTypes=@ work like the lowercase forms.
queryParams :: [(ByteString, Maybe ByteString)] -> [(Text, Text)]
queryParams qs = normalizeQueryParams [ (dec k, dec v) | (k, Just v) <- qs ]
  where
    dec = TE.decodeUtf8 . urlDecode True

-- | Milestone 17a: case-fold query parameter NAMES to lowercase, keeping
-- values (and order) as-is. Pure, exported for tests.
normalizeQueryParams :: [(Text, Text)] -> [(Text, Text)]
normalizeQueryParams = map (\(k, v) -> (T.toLower k, v))

-- | Case-insensitive substring match on 'itemTitle'. An empty search term
-- is a substring of every title, so @?search=@ behaves like no filter.
searchItems :: Text -> [MediaItem] -> [MediaItem]
searchItems term =
  filter (\i -> T.toLower term `T.isInfixOf` T.toLower (itemTitle i))

-- | Milestone 4n (docs/todo.org section 4): metadata-aware search --
-- case-insensitive substring match on the title OR on any person
-- ('metaPersons') of the item's metadata record. Items without metadata
-- match on title only.
searchItemsMeta :: (Text -> Maybe MediaMetadata) -> Text -> [MediaItem] -> [MediaItem]
searchItemsMeta metaLookup term items =
  filter (\i -> let t = T.toLower term
                in t `T.isInfixOf` T.toLower (itemTitle i)
                   || any (\p -> t `T.isInfixOf` T.toLower p)
                          (maybe [] metaPersons (metaLookup (itemId i)))) items

-- | Milestone 4n (docs/todo.org section 10): the "exclusive" filter --
-- keep only items whose id is in NO collection (the predicate says
-- whether an item id appears in some collection).
notInAnyCollection :: (Text -> Bool) -> [MediaItem] -> [MediaItem]
notInAnyCollection inAnyCollection = filter (not . inAnyCollection . itemId)

-- Milestone 4n (docs/todo.org section 4): image tags on the item JSON.

-- | @ImageTags@ object: @{type: tag}@ pairs, keys as stored
-- (e.g. @Primary@, @Backdrop@).
imageTagsValue :: [(Text, Text)] -> Value
imageTagsValue pairs = object [ Key.fromText ty .= tag | (ty, tag) <- pairs ]

-- | @BackdropImageTags@ array: the tags of all @Backdrop@ images
-- (case-insensitive type match).
backdropImageTagsValue :: [(Text, Text)] -> Value
backdropImageTagsValue pairs =
  toJSON [ tag | (ty, tag) <- pairs, T.toLower ty == "backdrop" ]

-- | @ImageBlurHashes@ object: @{type: hash}@ pairs, skipping empty
-- hashes (types whose blurhash could not be computed).
imageBlurHashesValue :: [(Text, Text)] -> Value
imageBlurHashesValue pairs =
  object [ Key.fromText ty .= h | (ty, h) <- pairs, not (T.null h) ]

-- | Merge blurhash placeholders into an item JSON object: adds
-- @ImageBlurHashes@ when there is at least one non-empty hash;
-- non-object values pass through unchanged.
attachImageBlurHashes :: [(Text, Text)] -> Value -> Value
attachImageBlurHashes pairs v = case v of
  Object o | any (not . T.null . snd) pairs ->
    Object (KeyMap.insert "ImageBlurHashes" (imageBlurHashesValue pairs) o)
  _ -> v

-- | Merge an item's custom tags (metadata 'metaTags', item tags API)
-- into its JSON object as a @Tags@ array. Attached only when the list is
-- non-empty; non-object values pass through unchanged. Callers apply
-- this BEFORE 'projectFields' so the @fields=@ projection can strip or
-- keep @Tags@ like any other key.
attachTags :: [Text] -> Value -> Value
attachTags tags v = case v of
  Object o | not (null tags) -> Object (KeyMap.insert "Tags" (toJSON tags) o)
  _                          -> v

-- | Merge image tags into an item JSON object: adds @ImageTags@ (all
-- pairs, or only the requested @imageType@ when given) and
-- @BackdropImageTags@ when backdrops exist. Attached only when there is
-- something to attach; non-object values pass through unchanged.
attachImageTags :: Maybe Text -> [(Text, Text)] -> Value -> Value
attachImageTags mOnlyType pairs v = case v of
  Object o ->
    let filtered = case mOnlyType of
          Just ty -> [ (t, g) | (t, g) <- pairs, T.toLower t == T.toLower ty ]
          Nothing -> pairs
        backdrops = [ g | (t, g) <- pairs, T.toLower t == "backdrop" ]
    in if null filtered && null backdrops
         then v
         else Object
                (KeyMap.insert "BackdropImageTags" (toJSON backdrops)
                  (KeyMap.insert "ImageTags" (imageTagsValue filtered) o))
  _ -> v

-- | Keep only items scanned from the given media folder (milestone 2c).
-- 'Nothing' (no @libraryId@ param) keeps everything.
filterByFolder :: Maybe Text -> [MediaItem] -> [MediaItem]
filterByFolder mFolder = case mFolder of
  Nothing  -> id
  Just fid -> filter (\i -> itemFolder i == fid)

-- | Milestone 17a: @?ParentId=<folderId>@ keeps only items whose
-- 'itemFolder' equals the value (exact 'Text' comparison). Folder ids
-- only - BoxSet pseudo-ids ("boxset-...") are NOT accepted here
-- (documented). Empty or absent param -> identity. Reads the
-- case-folded "parentid" key (see 'normalizeQueryParams').
filterByParent :: [(Text, Text)] -> [MediaItem] -> [MediaItem]
filterByParent rawParams items =
  case lookup "parentid" (normalizeQueryParams rawParams) of
    Just p | not (T.null p) -> filter (\i -> itemFolder i == p) items
    _                       -> items

-- | Milestone 17c: the @GET /Users/:id/Views@ entries - one per media
-- folder, Jellyfin CollectionFolder shape. Pure, exported for tests.
viewsFromFolders :: [MediaFolder] -> [Value]
viewsFromFolders folders =
  [ object
      [ "Name"           .= folderName f
      , "Id"             .= folderId f
      , "Type"           .= ("CollectionFolder" :: Text)
      , "CollectionType" .= folderType f
      ]
  | f <- folders
  ]

-- | Milestone 17c: the @GET /Shows/NextUp@ pure core - for every series
-- present in @items@, the first episode (by 'sortEpisodes' order) that is
-- NOT played according to the user-data lookup; series with no unplayed
-- episode are skipped (documented). With an empty lookup map every
-- series contributes its first episode. Pure, exported for tests.
nextUpEpisodes :: (Text -> Maybe UserData) -> [MediaItem] -> [MediaItem]
nextUpEpisodes lookupUd items =
  [ ep
  | (series, _count) <- showsFromItems items
  , let eps = sortEpisodes [ i | i <- items
                               , itemType i == "Episode"
                               , itemSeries i == Just series ]
  , ep <- take 1 [ e | e <- eps, not (maybe False udPlayed (lookupUd (itemId e))) ]
  ]

-- | Milestone 17c: the @GET /Users/:id/Items/Latest@ pure core - the
-- newest items by 'itemAddedAt' (DateCreated) descending, optionally
-- filtered by the @includeItemTypes@ param (comma-separated, see
-- 'filterByIncludeTypes'), capped at @limitCap@ (<= 0 -> default 12).
-- Pure, exported for tests.
latestItems :: [(Text, Text)] -> Int -> [MediaItem] -> [MediaItem]
latestItems rawParams limitCap items =
  take (if limitCap <= 0 then 12 else limitCap)
    (sortBy (comparing (Down . itemAddedAt))
            (filterByIncludeTypes (lookup "includeitemtypes" (normalizeQueryParams rawParams)) items))

-- | Keep only items of the given type (milestone 4d:
-- @?type=Movie@ / @?type=Episode@ / @?type=Audio@).
filterByType :: Maybe Text -> [MediaItem] -> [MediaItem]
filterByType mType = case mType of
  Nothing -> id
  Just t  -> filter (\i -> T.toCaseFold (itemType i) == T.toCaseFold (T.strip t))

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
      -- milestone 17b: the id key is "Id" now; compare case-insensitively so
      -- fields=Id / fields=id both keep it
      keep k = T.toCaseFold (Key.toText k) == "id" || T.toCaseFold (Key.toText k) `elem` wanted
  _ -> v

-- | Pure (milestone 7): does the effective type filter (@type@ or
-- @includeItemTypes@) include BoxSet, case-insensitively? When it does,
-- @\/items@ appends the synthesized BoxSet pseudo-items (collections).
typeFilterIncludesBoxSet :: [(Text, Text)] -> Bool
typeFilterIncludesBoxSet rawParams =
  let params = normalizeQueryParams rawParams in
  maybe False (\t -> T.toCaseFold (T.strip t) == "boxset") (lookup "type" params)
  || maybe False (\raw -> "boxset" `elem`
                    [ T.toCaseFold (T.strip x) | x <- T.splitOn "," raw ])
               (lookup "includeitemtypes" params)

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
applyMetadataFilters rawParams lookupMeta =
  foldl applyParam id
        [ ("genres", metaGenres), ("studios", metaStudios), ("persons", metaPersons) ]
  where
    params = normalizeQueryParams rawParams
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

-- | Milestone 5f: is this @sortBy@ value a user-data sort key (handled by
-- 'sortByUserDataField' instead of 'sortItems')? Case-insensitive.
isUserDataSortKey :: Text -> Bool
isUserDataSortKey s = T.toCaseFold (T.strip s) `elem` ["lastplayeddate", "playcount"]

-- | Pure (milestone 5f): sort items by a per-user 'UserData' field.
-- @lastPlayedDate@ sorts by 'udLastPlayedAt' descending with missing
-- records sorting LAST (regardless of direction); @playCount@ sorts by
-- 'udPlayCount' descending (missing = 0). Both directions are descending
-- (most-recent / highest first, the Jellyfin default); any other key is
-- the identity. The sort is stable ('sortBy'): equal keys keep their
-- relative order, so the caller can pre-sort this way before
-- 'applyItemQuery' paginates (which skips its own sort for these keys).
sortByUserDataField
  :: Text                     -- ^ sortBy value
  -> (Text -> Maybe UserData) -- ^ itemId -> 'UserData' lookup
  -> [MediaItem]
  -> [MediaItem]
sortByUserDataField sortKey lookupItems items =
  case T.toCaseFold (T.strip sortKey) of
    "lastplayeddate" -> sortBy cmpLastPlayed items
    "playcount"      -> sortBy cmpPlayCount items
    _                -> items
  where
    cmpLastPlayed x y =
      case (udLastPlayedAt =<< lookupItems (itemId x), udLastPlayedAt =<< lookupItems (itemId y)) of
        (Nothing, Nothing) -> EQ
        (Nothing, Just _)  -> GT   -- unplayed sorts last
        (Just _, Nothing)  -> LT
        (Just a, Just b)   -> compare b a   -- descending
    cmpPlayCount x y =
      compare (pc y) (pc x)                 -- descending
      where pc i = maybe 0 udPlayCount (lookupItems (itemId i))

-- | Pure (milestone 4i, Jellyfin =/items/:id/Similar=): items sharing at
-- least one genre (case-insensitive) with the base item, excluding the
-- base item itself, ranked by shared-genre count DESC then title ASC,
-- capped at @limit@ (limit <= 0 means the default 12, applied here).
-- Items without metadata never match; a base item without metadata
-- yields []. All item types are considered (Jellyfin mixes types).
similarItems
  :: (Text -> Maybe MediaMetadata)  -- ^ itemId -> metadata lookup
  -> Int                            -- ^ limit (<= 0 -> 12)
  -> MediaItem                      -- ^ base item
  -> [MediaItem]                    -- ^ candidate pool
  -> [MediaItem]
similarItems lookupMeta limit base allItems =
  case lookupMeta (itemId base) of
    Nothing -> []
    Just bm ->
      let baseGenres = map T.toCaseFold (metaGenres bm)
          baseKeys   = map T.toCaseFold (metaKeywords bm)   -- m53a: keywords count too
          shared i = case lookupMeta (itemId i) of
            Nothing -> (0, 0)
            Just m  -> ( length (filter (`elem` baseGenres) (map T.toCaseFold (metaGenres m)))
                       , length (filter (`elem` baseKeys) (map T.toCaseFold (metaKeywords m))) )
          scored = [ (g + k, itemTitle i, i)
                   | i <- allItems
                   , itemId i /= itemId base
                   , let (g, k) = shared i
                   , g + k > 0
                   ]
          sorted = sortBy (\(a, ta, _) (b, tb, _) -> compare b a <> compare ta tb) scored
          lim = if limit <= 0 then 12 else limit
      in take lim (map (\(_, _, i) -> i) sorted)

-- | Pure (milestone 4j): parse the @version=N@ query param (0-based index
-- into the item's version list). Absent -> 'Right' 0 (the primary);
-- unparseable -> 'Left' (the caller turns it into a 400).
versionParam :: [(Text, Text)] -> Either Text Int
versionParam qparams =
  case lookup "version" (normalizeQueryParams qparams) of
    Nothing -> Right 0
    Just v  -> maybe (Left "invalid version parameter") Right (readMaybe (T.unpack v))

-- | Pure (milestone 4j): pick the file path for version index @n@ of an
-- item. When the item has no version list, only index 0 (the primary
-- 'itemPath') exists. Out-of-range -> 'Left' (400).
versionPathFor :: MediaItem -> Int -> Either Text FilePath
versionPathFor item n
  | n < 0 = Left "version must be >= 0"
  | null (itemVersions item) =
      if n == 0 then Right (itemPath item) else Left "no such version"
  | n < length (itemVersions item) = Right (itemVersions item !! n)
  | otherwise = Left "no such version"

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
      Just o | T.toCaseFold (T.strip o) `elem` ["desc", "descending"] -> \x y -> base y x
      _           -> base  -- "asc", "ascending", anything else, Nothing

-- | Pagination window: @startIndex@ (default 0, clamped to >= 0) and
-- @limit@ (default: all remaining items). A negative limit yields an
-- empty page ('take' of a negative count).
paginate :: Maybe Int -> Maybe Int -> [a] -> [a]
paginate mStart mLimit xs =
  let start = max 0 (fromMaybe 0 mStart)
  in case mLimit of
       Nothing -> drop start xs
       Just l  -> take l (drop start xs)

-- Input-validation hardening (docs/todo.org section 14): pure clamps for
-- numeric query params, so no request can cause absurd work. Exported for
-- tests.

-- | Pure: clamp to >= 0.
clampNonNegative :: Int -> Int
clampNonNegative = max 0

-- | Pure: @startIndex@ clamp — absent -> 0, negative -> 0.
clampStartIndex :: Maybe Int -> Int
clampStartIndex = maybe 0 clampNonNegative

-- | Pure: @limit@ clamp — absent -> default 100; present -> clamped to
-- [1, 1000]. (Callers with their own default wrap it:
-- @clampLimit (Just \<$> m)@ keeps 'Nothing' -> 100; use
-- @clampLimit (Just (fromMaybe d m))@ to keep a custom default.)
clampLimit :: Maybe Int -> Int
clampLimit Nothing  = 100
clampLimit (Just n) = min 1000 (max 1 n)

-- | Pure: parse a numeric query value, clamped to [lo, hi]; an
-- unparseable value yields the default (never 'Nothing', so callers can
-- rely on a sane int). Whitespace is stripped first.
parseBoundedInt :: Int -> Int -> Int -> Text -> Maybe Int
parseBoundedInt lo hi def t =
  case readMaybe (T.unpack (T.strip t)) of
    Just n  -> Just (min hi (max lo n))
    Nothing -> Just def

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
  let get name = lookup (T.toLower name) (normalizeQueryParams params)
      searchTerm = case get "search" of
        Just s  -> Just s
        Nothing -> get "searchTerm"          -- milestone 4e alias
      searched = maybe items (`searchItems` items) searchTerm
      inFolder = filterByFolder (get "libraryId") searched
      typed    = filterByType (get "type") inFolder
      series   = filterBySeries (get "series") typed
      incTypes = filterByIncludeTypes (get "includeItemTypes") series
      years    = filterByYears (get "years") incTypes
      -- Milestone 5f: user-data sort keys (lastPlayedDate/playCount) are
      -- pre-sorted by the caller via 'sortByUserDataField'; skip the plain
      -- sort here so the pre-sorted order survives into pagination.
      sorted   = case get "sortBy" of
        Just s | isUserDataSortKey s -> years
        _                            -> sortItems (get "sortBy") (get "sortOrder") years
      total    = length sorted
      -- hardening: clamp explicit numeric params (negative start -> 0,
      -- limit -> [1, 1000]); absent limit keeps the paginate default
      start    = get "startIndex" >>= readMaybe . T.unpack
      limit    = get "limit" >>= readMaybe . T.unpack
  in (total, paginate (clampNonNegative <$> start) (min 1000 . max 1 <$> limit) sorted)

-- | m40: add @DisplayTitle@ to a PascalCase item JSON object -- the
-- metadata title when the lookup yields a non-empty one, otherwise the
-- existing @Name@ value. Non-object values pass through unchanged.
withDisplayTitle :: (Text -> Maybe Text) -> Value -> Value
withDisplayTitle metaTitleOf v = case v of
  Object o ->
    let mId = case KeyMap.lookup (Key.fromText "Id") o of
                Just (String t) -> Just t
                _               -> Nothing
        fallback = case KeyMap.lookup (Key.fromText "Name") o of
                     Just (String t) -> Just t
                     _               -> Nothing
        disp = case mId >>= metaTitleOf of
                 Just t | not (T.null t) -> Just t
                 _                       -> fallback
    in case disp of
         Just t  -> Object (KeyMap.insert (Key.fromText "DisplayTitle") (String t) o)
         Nothing -> v
  _ -> v

-- | m40: lowercase variant for the legacy simple item shape
-- (@id@/@title@): adds @displayTitle@ (metadata title, else @title@).
withDisplayTitleLower :: (Text -> Maybe Text) -> Value -> Value
withDisplayTitleLower metaTitleOf v = case v of
  Object o ->
    let mId = case KeyMap.lookup (Key.fromText "id") o of
                Just (String t) -> Just t
                _               -> Nothing
        fallback = case KeyMap.lookup (Key.fromText "title") o of
                     Just (String t) -> Just t
                     _               -> Nothing
        disp = case mId >>= metaTitleOf of
                 Just t | not (T.null t) -> Just t
                 _                       -> fallback
    in case disp of
         Just t  -> Object (KeyMap.insert (Key.fromText "displayTitle") (String t) o)
         Nothing -> v
  _ -> v

-- | m43: add the lowercase @type@ (and @seriesName@ when present) keys to
-- a legacy simple item JSON object, so the Elm UI can split Movies/Shows.
withLegacyType :: MediaItem -> Value -> Value
withLegacyType i v = case v of
  Object o ->
    let o1 = KeyMap.insert (Key.fromText "type") (String (itemType i)) o
        o2 = case itemSeries i of
               Just sn -> KeyMap.insert (Key.fromText "seriesName") (String sn) o1
               Nothing -> o1
        -- m47: season/episode numbers so the UI can match series episodes
        o3 = case itemSeason i of
               Just se -> KeyMap.insert (Key.fromText "seasonNumber") (Number (fromIntegral se)) o2
               Nothing -> o2
        o4 = case itemEpisode i of
               Just ep -> KeyMap.insert (Key.fromText "episodeNumber") (Number (fromIntegral ep)) o3
               Nothing -> o3
    in Object o4
  _ -> v

-- | m43: per-item display title -- the metadata title when a non-empty one
-- exists, else an episode label @Series S01E02@ for episodes, else 'Nothing'
-- (the caller then falls back to the raw title).
displayTitleForItem :: HM.HashMap Text MediaMetadata -> MediaItem -> Maybe Text
displayTitleForItem metaMap i =
  case HM.lookup (itemId i) metaMap >>= metaTitle of
    Just t | not (T.null t) -> Just t
    _ -> case (itemType i, itemSeries i, itemSeason i, itemEpisode i) of
      ("Episode", Just sn, Just se, Just ep) ->
        Just (sn <> T.pack (" S" ++ pad2 se ++ "E" ++ pad2 ep))
      _ -> Nothing
  where
    pad2 n = let str = show n in if length str < 2 then '0' : str else str

-- | m43: full legacy-row enrichment for the stamno-media UI: type /
-- seriesName keys plus the episode-aware displayTitle.
withLegacyItem :: HM.HashMap Text MediaMetadata -> MediaItem -> Value -> Value
withLegacyItem metaMap i = withLegacyGenres metaMap i . withDisplayTitleLower lk . withLegacyType i
  where
    lk ident =
      if ident == itemId i
        then displayTitleForItem metaMap i
        else HM.lookup ident metaMap >>= metaTitle
    -- m53a: expose the genre list on legacy rows so the UI can filter
    withLegacyGenres mm it v = case v of
      Object o -> case HM.lookup (itemId it) mm of
        Just m | not (null (metaGenres m)) ->
          Object (KeyMap.insert (Key.fromText "genres") (toJSON (metaGenres m)) o)
        _ -> v
      _ -> v
