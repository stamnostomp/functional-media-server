module FMS.Modules.Indexers.Types
  ( Indexer (..)
  , indexerPublicJson
  , indexerSearchUrl
  , categoriesParam
  , base64UrlEncode
  , base64UrlDecode
  , hostOf
  , hostMatches
  , defaultCategories
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value, object, withObject, (.:), (.:?), (.!=), (.=))
import qualified Data.Aeson as Aeson
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Text (Text)
import Data.Word (Word8)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime)
import Network.HTTP.Types (urlEncode)

import FMS.Modules.Requests.Push (base64Encode)

-- | Default Torznab categories for a seeded/configured indexer: movies
-- (2000) + TV (5000).
defaultCategories :: [Int]
defaultCategories = [2000, 5000]

-- | One direct Torznab/Newznab indexer (m21). The API key is SECRET: it
-- is only ever sent to the indexer itself and is redacted from non-admin
-- API responses ('indexerPublicJson').
data Indexer = Indexer
  { indexerId         :: !Text
  , indexerName       :: !Text
  , indexerBaseUrl    :: !Text          -- ^ e.g. http://127.0.0.1:9117
  , indexerApiKey     :: !Text
  , indexerCategories :: ![Int]         -- ^ Torznab cats, default [2000,5000]
  , indexerType       :: !Text          -- ^ "torznab" | "newznab"
  , indexerEnabled    :: !Bool
  , indexerLastTestAt :: !(Maybe UTCTime)
  , indexerLastTestOk :: !(Maybe Bool)
  } deriving (Eq, Show)

instance ToJSON Indexer where
  toJSON i = object
    [ "Id"         .= indexerId i
    , "Name"       .= indexerName i
    , "BaseUrl"    .= indexerBaseUrl i
    , "ApiKey"     .= indexerApiKey i
    , "Categories" .= indexerCategories i
    , "Type"       .= indexerType i
    , "Enabled"    .= indexerEnabled i
    , "LastTestAt" .= indexerLastTestAt i
    , "LastTestOk" .= indexerLastTestOk i
    ]

instance FromJSON Indexer where
  parseJSON = withObject "Indexer" $ \o ->
    Indexer
      <$> o .:  "Id"
      <*> o .:  "Name"
      <*> o .:  "BaseUrl"
      <*> o .:? "ApiKey" .!= ""
      <*> o .:? "Categories" .!= defaultCategories
      <*> o .:? "Type" .!= "torznab"
      <*> o .:? "Enabled" .!= True
      <*> o .:? "LastTestAt"
      <*> o .:? "LastTestOk"

-- | API JSON with the ApiKey redacted (null) for non-admins; admins get
-- the full record. SECURITY: never expose the key to non-admins.
indexerPublicJson :: Bool -> Indexer -> Value
indexerPublicJson isAdmin i
  | isAdmin   = toJSON i
  | otherwise = object
      [ "Id"         .= indexerId i
      , "Name"       .= indexerName i
      , "BaseUrl"    .= indexerBaseUrl i
      , "ApiKey"     .= Aeson.Null
      , "Categories" .= indexerCategories i
      , "Type"       .= indexerType i
      , "Enabled"    .= indexerEnabled i
      , "LastTestAt" .= indexerLastTestAt i
      , "LastTestOk" .= indexerLastTestOk i
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
-- @<base>/api?t=search&q=<enc>&apikey=<key>&cat=<cats>@ for torznab,
-- @<base>/api?t=search&q=<enc>&apikey=<key>@ for newznab.
indexerSearchUrl :: Indexer -> Maybe Text -> Text -> String
indexerSearchUrl idx mType q =
  T.unpack (T.dropWhileEnd (== '/') (indexerBaseUrl idx))
    ++ "/api?t=search&q=" ++ T.unpack (urlEncodeText q)
    ++ "&apikey=" ++ T.unpack (indexerApiKey idx)
    ++ catPart
  where
    catPart
      | indexerType idx == "newznab" = ""
      | otherwise = "&cat=" ++ T.unpack (T.intercalate "," (map (T.pack . show) (categoriesParam (indexerCategories idx) mType)))

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
