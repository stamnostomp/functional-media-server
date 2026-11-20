module FMS.Modules.Indexers.Types
  ( Indexer (..)
  , indexerFullJson
  , indexerPublicJson
  , indexerSearchUrl
  , indexerSearchUrlExtra
  , torrentDaySearchUrl
  , parseTorrentDayJson
  , categoriesParam
  , base64UrlEncode
  , base64UrlDecode
  , hostOf
  , hostMatches
  , indexerCookieHeader
  , defaultCategories
  , defaultSearchPath
  , validAuthType
  , validResponseFormat
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, (.=))
import Data.Aeson.Types (Parser)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Char (toLower)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime)
import Data.Word (Word8)
import Network.HTTP.Types (urlEncode)

import FMS.Modules.Requests.Push (base64Encode)
import FMS.Modules.Trackers.Types (Release (..))

-- | Default Torznab categories for a seeded/configured indexer: movies
-- (2000) + TV (5000).
defaultCategories :: [Int]
defaultCategories = [2000, 5000]

-- | Default Torznab/Newznab search path (Jackett-style @/api@).
defaultSearchPath :: Text
defaultSearchPath = "/api"

-- | Valid 'indexerAuthType' values: @apikey@ (default) or @cookie@ (m23).
validAuthType :: Text -> Bool
validAuthType t = T.toLower t `elem` ["apikey", "cookie"]

-- | Valid 'indexerResponseFormat' values: @torznab@ (default XML) or
-- @torrentday-json@ (TorrentDay t.json JSON array, m23).
validResponseFormat :: Text -> Bool
validResponseFormat t = T.toLower t `elem` ["torznab", "torrentday-json"]

-- | One direct indexer (m21, extended m23 with cookie auth + t.json).
-- ApiKey AND Cookie are SECRET: they are only ever sent to the indexer
-- itself and are redacted from non-admin API responses
-- ('indexerPublicJson').
data Indexer = Indexer
  { indexerId             :: !Text
  , indexerName           :: !Text
  , indexerBaseUrl        :: !Text          -- ^ e.g. http://127.0.0.1:9117
  , indexerApiKey         :: !Text
  , indexerCategories     :: ![Int]         -- ^ Torznab cats, default [2000,5000]
  , indexerType           :: !Text          -- ^ "torznab" | "newznab"
  , indexerEnabled        :: !Bool
  , indexerLastTestAt     :: !(Maybe UTCTime)
  , indexerLastTestOk     :: !(Maybe Bool)
  , indexerAuthType       :: !Text          -- ^ "apikey" (default) | "cookie" (m23)
  , indexerCookie         :: !Text          -- ^ Cookie header value for cookie auth (m23)
  , indexerResponseFormat :: !Text          -- ^ "torznab" (default) | "torrentday-json" (m23)
  , indexerSearchPath     :: !Text          -- ^ torznab search path, default "/api" (m23)
  } deriving (Eq, Show)

-- | Persist with LOWERCASE keys (m23); 'fieldDef' decodes both the new
-- lowercase keys and the old PascalCase keys, so pre-m23 indexers.json
-- files keep working and missing fields get defaults.
instance ToJSON Indexer where
  toJSON i = object
    [ "id"             .= indexerId i
    , "name"           .= indexerName i
    , "baseUrl"        .= indexerBaseUrl i
    , "apiKey"         .= indexerApiKey i
    , "categories"     .= indexerCategories i
    , "type"           .= indexerType i
    , "enabled"        .= indexerEnabled i
    , "lastTestAt"     .= indexerLastTestAt i
    , "lastTestOk"     .= indexerLastTestOk i
    , "authType"       .= indexerAuthType i
    , "cookie"         .= indexerCookie i
    , "responseFormat" .= indexerResponseFormat i
    , "searchPath"     .= indexerSearchPath i
    ]

-- | Read a field accepting both PascalCase (old files) and camelCase
-- lowercase-first (new files) keys, with a default when absent.
fieldDef :: FromJSON a => a -> Text -> KeyMap.KeyMap Value -> Parser a
fieldDef def name o =
  case KeyMap.lookup (Key.fromText name) o of
    Just v  -> parseJSON v
    Nothing -> case KeyMap.lookup (Key.fromText (lowerFirst name)) o of
      Just v  -> parseJSON v
      Nothing -> pure def
  where
    lowerFirst t = case T.uncons t of
      Just (c, rest) -> T.cons (toLower c) rest
      Nothing        -> t

instance FromJSON Indexer where
  parseJSON = withObject "Indexer" $ \o ->
    Indexer
      <$> fieldDef "" "Id" o
      <*> fieldDef "" "Name" o
      <*> fieldDef "" "BaseUrl" o
      <*> fieldDef "" "ApiKey" o
      <*> fieldDef defaultCategories "Categories" o
      <*> fieldDef "torznab" "Type" o
      <*> fieldDef True "Enabled" o
      <*> fieldDef Nothing "LastTestAt" o
      <*> fieldDef Nothing "LastTestOk" o
      <*> fieldDef "apikey" "AuthType" o
      <*> fieldDef "" "Cookie" o
      <*> fieldDef "torznab" "ResponseFormat" o
      <*> fieldDef defaultSearchPath "SearchPath" o

-- | Full API JSON (PascalCase, includes ApiKey/Cookie) for ADMINS only.
-- Persistence uses the lowercase 'ToJSON' instance instead.
indexerFullJson :: Indexer -> Value
indexerFullJson i = object
  [ "Id"             .= indexerId i
  , "Name"           .= indexerName i
  , "BaseUrl"        .= indexerBaseUrl i
  , "ApiKey"         .= indexerApiKey i
  , "Categories"     .= indexerCategories i
  , "Type"           .= indexerType i
  , "Enabled"        .= indexerEnabled i
  , "LastTestAt"     .= indexerLastTestAt i
  , "LastTestOk"     .= indexerLastTestOk i
  , "AuthType"       .= indexerAuthType i
  , "Cookie"         .= indexerCookie i
  , "ResponseFormat" .= indexerResponseFormat i
  , "SearchPath"     .= indexerSearchPath i
  ]

-- | API JSON with ApiKey AND Cookie redacted (null) for non-admins;
-- admins get the full record. SECURITY: never expose either secret to
-- non-admins.
indexerPublicJson :: Bool -> Indexer -> Value
indexerPublicJson isAdmin i
  | isAdmin   = indexerFullJson i
  | otherwise = object
      [ "Id"             .= indexerId i
      , "Name"           .= indexerName i
      , "BaseUrl"        .= indexerBaseUrl i
      , "ApiKey"         .= Aeson.Null
      , "Categories"     .= indexerCategories i
      , "Type"           .= indexerType i
      , "Enabled"        .= indexerEnabled i
      , "LastTestAt"     .= indexerLastTestAt i
      , "LastTestOk"     .= indexerLastTestOk i
      , "AuthType"       .= indexerAuthType i
      , "Cookie"         .= Aeson.Null
      , "ResponseFormat" .= indexerResponseFormat i
      , "SearchPath"     .= indexerSearchPath i
      ]

-- | Effective Torznab cat list for a search: when the caller asks for
-- @type=movie@ (cat 2000) or @type=tv@ (cat 5000), narrow the indexer's
-- categories to that one (falling back to the single cat when the
-- indexer does not list it); otherwise use the indexer's categories.
categoriesParam :: [Int] -> Maybe Text -> [Int]
categoriesParam cats mType =
  case mType of
    Just t
      | T.toLower t == "movie" -> pick 2000
      | T.toLower t == "tv"    -> pick 5000
      | otherwise              -> cats
    Nothing -> cats
  where
    pick c = case filter (== c) cats of
      [] -> [c]
      xs -> xs

-- | Pure: build the Torznab/Newznab search URL for a DIRECT indexer
-- (FMS itself acts as the aggregator, Jackett-style):
-- @<base><searchPath>?t=search&q=<enc>&apikey=<key>&cat=<cats>@ for
-- newznab/torznab with apikey auth; when @authType=cookie@ the apikey
-- param is omitted (the Cookie header carries the auth instead, see
-- 'indexerCookieHeader'). @cat@ is dropped for newznab.
indexerSearchUrl :: Indexer -> Maybe Text -> Text -> String
indexerSearchUrl idx mType q =
  indexerSearchUrlExtra idx mType q []

-- | m60: like 'indexerSearchUrl' but appends extra torznab params
-- (@season@, @ep@) when present — used for episode / season-pack
-- searches from the Discover tab. Keys/values are percent-encoded.
indexerSearchUrlExtra :: Indexer -> Maybe Text -> Text -> [(Text, Text)] -> String
indexerSearchUrlExtra idx mType q extras =
  T.unpack (T.dropWhileEnd (== '/') (indexerBaseUrl idx))
    ++ T.unpack (indexerSearchPath idx)
    ++ "?t=search&q=" ++ T.unpack (urlEncodeText q)
    ++ keyPart
    ++ catPart
    ++ concatMap (\(k, v) -> "&" ++ T.unpack k ++ "=" ++ T.unpack (urlEncodeText v)) extras
  where
    keyPart
      | indexerAuthType idx == "cookie" = ""
      | otherwise = "&apikey=" ++ T.unpack (indexerApiKey idx)
    catPart
      | indexerType idx == "newznab" = ""
      | otherwise = "&cat=" ++ T.unpack (T.intercalate "," (map (T.pack . show) (categoriesParam (indexerCategories idx) mType)))

-- | Pure: TorrentDay t.json search URL (m23):
-- @<base>/t.json?<cat1>;<cat2>;...;q=<enc>@ -- TorrentDay takes the
-- categories as bare semicolon-separated params before @q@.
torrentDaySearchUrl :: Indexer -> Text -> String
torrentDaySearchUrl idx q =
  T.unpack (T.dropWhileEnd (== '/') (indexerBaseUrl idx))
    ++ "/t.json?"
    ++ (if null cats then "" else T.unpack (T.intercalate ";" (map (T.pack . show) cats)) ++ ";")
    ++ "q=" ++ T.unpack (urlEncodeText q)
  where
    cats = indexerCategories idx

-- | m67: TorrentDay emits the INVALID JSON escape @\\'@ for apostrophes in
-- names (e.g. @Pan\\'s Labyrinth@), which makes Aeson.decode reject the
-- WHOLE array -- every search whose results contain an apostrophe silently
-- yields []. Rewrite @\\'@ to @'@ before decoding (strictly invalid JSON
-- anyway, so this only ever repairs).
sanitizeTorrentDayJson :: LBS.ByteString -> LBS.ByteString
sanitizeTorrentDayJson = LBS.fromStrict . go . LBS.toStrict
  where
    pat = BSC.pack "\\'"  -- the 2-byte sequence backslash + quote
    go bs = case BS.breakSubstring pat bs of
      (pre, rest)
        | BS.null rest -> pre
        | otherwise    -> pre <> BSC.pack "'" <> go (BS.drop (BS.length pat) rest)

-- | Pure: parse a TorrentDay t.json response (a JSON array of rows) into
-- releases. Each row looks like
-- @{\"name\":\"...\",\"t\":7,\"size\":2000000000,\"seeders\":9,\"c\":1,\"ctime\":...}@;
-- the .torrent download URL is derived as
-- @<base>/download.php/<t>/<t>.torrent@. Missing/unknown fields are
-- tolerated; a non-array body yields []. m67: the body is sanitized
-- first ('sanitizeTorrentDayJson').
parseTorrentDayJson :: Text -> LBS.ByteString -> [Release]
parseTorrentDayJson baseUrl bs =
  case Aeson.decode (sanitizeTorrentDayJson bs) of
    Just (Aeson.Array rows) -> mapMaybeRow rows
    _                       -> []
  where
    mapMaybeRow rows = [ r | Aeson.Object o <- foldr (:) [] rows, Just r <- [rowToRelease o] ]
    rowToRelease o = do
      name <- KeyMap.lookup (Key.fromText "name") o >>= asText
      t <- KeyMap.lookup (Key.fromText "t") o >>= asInt
      let sizeB = KeyMap.lookup (Key.fromText "size") o >>= asInt
          seeders = fromIntegral <$> (KeyMap.lookup (Key.fromText "seeders") o >>= asInt) :: Maybe Int
          dl = baseUrl' <> "/download.php/" <> T.pack (show t) <> "/" <> T.pack (show t) <> ".torrent"
      pure Release
        { relTitle = name
        , relSizeBytes = sizeB
        , relSeeders = seeders
        , relDownloadUrl = Just dl
        , relMagnet = Nothing
        }
    baseUrl' = T.dropWhileEnd (== '/') baseUrl
    asText (Aeson.String s) = Just s
    asText _ = Nothing
    asInt (Aeson.Number n) = Just (round n :: Integer)
    asInt _ = Nothing

-- | The Cookie header value for cookie-auth indexers (m23), if any.
indexerCookieHeader :: Indexer -> [(Text, Text)]
indexerCookieHeader idx
  | indexerAuthType idx == "cookie" && not (T.null (indexerCookie idx)) =
      [("Cookie", indexerCookie idx)]
  | otherwise = []

-- | Percent-encode a query-string value (http-types 'urlEncode' query mode).
urlEncodeText :: Text -> Text
urlEncodeText = T.pack . BSC.unpack . urlEncode True . TE.encodeUtf8

-- | Pure: base64url (RFC 4648 §5, no padding) of the UTF-8 bytes. Used as
-- the opaque @r=<releaseId>@ token for proxied downloads: it is
-- reversible ONLY server-side, so raw indexer URLs (which embed the
-- apikey) never appear in API responses.
base64UrlEncode :: Text -> Text
base64UrlEncode txt =
  T.filter (/= '=') (T.map tr (base64Encode (TE.encodeUtf8 txt)))
  where
    tr '+' = '-'
    tr '/' = '_'
    tr c  = c

-- | Pure: decode a base64url token back to the original text.
-- 'Nothing' on any invalid character or bad length.
base64UrlDecode :: Text -> Maybe Text
base64UrlDecode t0 =
  let t = T.map tr t0
      tr '-' = '+'
      tr '_' = '/'
      tr c  = c
      pad = (4 - (T.length t `mod` 4)) `mod` 4
      padded = t <> T.replicate pad "="
  in if T.length t `mod` 4 == 1 || not (T.all valid padded)
       then Nothing
       else decodeBase64 padded
  where
    valid c = (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')
              || (c >= '0' && c <= '9') || c == '+' || c == '/' || c == '='

decodeBase64 :: Text -> Maybe Text
decodeBase64 t =
  if T.null t
    then Just ""
    else do
      bytes <- go (T.unpack t) []
      pure (TE.decodeUtf8 (BS.pack bytes))
  where
    val :: Char -> Int
    val c
      | c >= 'A' && c <= 'Z' = fromEnum c - fromEnum 'A'
      | c >= 'a' && c <= 'z' = fromEnum c - fromEnum 'a' + 26
      | c >= '0' && c <= '9' = fromEnum c - fromEnum '0' + 52
      | c == '+' = 62
      | c == '/' = 63
      | otherwise = -1
    go :: String -> [Word8] -> Maybe [Word8]
    go [] acc = pure acc
    go (a:b:c:d:rest) acc =
      let va = val a; vb = val b; vc = val c; vd = val d
      in if va < 0 || vb < 0 || (c /= '=' && vc < 0) || (d /= '=' && vd < 0)
           then Nothing
           else do
             let n = (va `shiftL` 18) .|. (vb `shiftL` 12) .|. ((if c == '=' then 0 else vc) `shiftL` 6) .|. (if d == '=' then 0 else vd)
                 b1 = fromIntegral ((n `shiftR` 16) .&. 255) :: Word8
                 b2 = fromIntegral ((n `shiftR` 8) .&. 255) :: Word8
                 b3 = fromIntegral (n .&. 255) :: Word8
             if c == '=' then
               if d == '=' then go rest (acc ++ [b1])
               else Nothing
             else if d == '=' then go rest (acc ++ [b1, b2])
             else go rest (acc ++ [b1, b2, b3])
    go _ _ = Nothing

-- | Pure: host (lowercased, no port, no userinfo) of a URL.
-- @hostOf \"https://user:pw@Example.COM:8443/x\" == \"example.com\"@.
hostOf :: Text -> Text
hostOf url =
  let noScheme = case T.breakOn "://" url of
        (_, rest) | not (T.null rest) -> T.drop 3 rest
        _                             -> url
      authority = T.takeWhile (\c -> c /= '/' && c /= '?' && c /= '#') noScheme
      noUser = if T.elem '@' authority
                 then snd (T.breakOnEnd "@" authority)  -- text after the last '@'
                 else authority
  in T.toLower (T.takeWhile (/= ':') noUser)

-- | Pure: SSRF guard — does the raw download URL's host match the
-- indexer's base URL host? Empty hosts never match.
hostMatches :: Text -> Text -> Bool
hostMatches rawUrl baseUrl =
  let h1 = hostOf rawUrl
      h2 = hostOf baseUrl
  in not (T.null h1) && h1 == h2
