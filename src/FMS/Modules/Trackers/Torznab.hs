module FMS.Modules.Trackers.Torznab
  ( jackettSearchUrl
  , parseTorznabResults
  , jackettSearch
  , fetchUrl
  , decompressBody
  ) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import qualified Codec.Compression.GZip as GZip
import qualified Codec.Compression.Zlib as Zlib
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Text.Read (decimal)
import Network.HTTP.Client (managerResponseTimeout, requestHeaders, responseBody, responseHeaders, responseStatus, responseTimeoutMicro)
import Network.HTTP.Conduit (httpLbs, newManager, parseRequest_)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (Header, ResponseHeaders, hContentEncoding, statusCode, urlEncode)
import Text.XML (parseLBS)
import qualified Text.XML as XML
import Text.XML.Cursor (Cursor, attribute, child, content, descendant, fromDocument, laxElement)

import FMS.Modules.Trackers.Types (Release (..))

-- | Response-header timeout for tracker calls: 10s in microseconds.
trackerTimeoutMicros :: Int
trackerTimeoutMicros = 10000000

-- | Pure: build the Jackett Torznab search URL for one tracker:
-- @<url>/api/v2.0/indexers/<id>/results?apikey=<key>&t=search&q=<enc>[@&cat=2000|5000]@.
-- @cat@ is 2000 (movies) or 5000 (TV) when a media type is given.
jackettSearchUrl :: Text -> Text -> Text -> Text -> Maybe Text -> String
jackettSearchUrl base trackerId apiKey q mType =
  T.unpack (T.dropWhileEnd (== '/') base)
    ++ "/api/v2.0/indexers/" ++ T.unpack trackerId
    ++ "/results?apikey=" ++ T.unpack apiKey
    ++ "&t=search&q=" ++ T.unpack (urlEncodeText q)
    ++ maybe "" (\t -> "&cat=" ++ catFor t) mType
  where
    catFor "tv" = "5000"
    catFor _    = "2000"

-- | Percent-encode a query-string value (http-types 'urlEncode' query mode).
urlEncodeText :: Text -> Text
urlEncodeText = T.pack . BSC.unpack . urlEncode True . TE.encodeUtf8

-- | Pure: parse a Torznab RSS/XML response into releases. Reads, per
-- @<item>@: @<title>@, @<size>@ (bytes), @<attr name="seeders" value=".."/>@,
-- @<attr name="download" value=".."/>@ or @<enclosure url=".."/>@, and
-- @<attr name="magneturl" value=".."/>@. Items without a title are
-- skipped; a non-XML body yields [].
parseTorznabResults :: LBS.ByteString -> [Release]
parseTorznabResults bs =
  case parseLBS XML.def bs of
    Left _    -> []
    Right doc -> mapMaybe itemToRelease (descendant (fromDocument doc) >>= laxElement "item")

-- | Child elements of @c@ with the given local name ('laxElement' filters
-- the node it is applied to, so go through 'child' first, like Nfo.hs).
childElems :: Text -> Cursor -> [Cursor]
childElems name c = child c >>= laxElement name

-- | Text content of the first child element with the given local name.
childText :: Text -> Cursor -> Maybe Text
childText name c =
  listToMaybe [ t | e <- childElems name c, t <- child e >>= content ]

itemToRelease :: Cursor -> Maybe Release
itemToRelease c =
  case childText "title" c of
    Nothing -> Nothing
    Just t  ->
      let attrs = [ (T.toLower n, v)
                  | a <- childElems "attr" c
                  , Just n <- [listToMaybe (attribute (XML.Name "name" Nothing Nothing) a)]
                  , Just v <- [listToMaybe (attribute (XML.Name "value" Nothing Nothing) a)]
                  ]
          enclosureUrl = listToMaybe
            [ u | e <- childElems "enclosure" c
                , Just u <- [listToMaybe (attribute (XML.Name "url" Nothing Nothing) e)] ]
      in Just Release
           { relTitle = T.strip t
           , relSizeBytes = childText "size" c >>= readInteger
           , relSeeders = lookup "seeders" attrs >>= readInt
           , relDownloadUrl = lookup "download" attrs <|> enclosureUrl
           , relMagnet = lookup "magneturl" attrs
           }
  where
    readInteger t = case decimal t of
      Right (n, rest) | T.null rest -> Just n
      _ -> Nothing
    readInt t = case decimal t of
      Right (n, rest) | T.null rest -> Just (fromIntegral (n :: Integer))
      _ -> Nothing

-- | m67: decode the response body when it is STILL compressed. Note
-- http-client already auto-decompresses gzip when it advertised it (but
-- leaves the stale @Content-Encoding@ header in place), so the header
-- alone is not enough -- check the magic bytes too: a body starting
-- @\\x1f\\x8b@ is gzip, @\\x78@ is zlib/deflate; anything else is passed
-- through raw. Without this, indexers that always gzip (TorrentDay behind
-- lighttpd) when the client did NOT ask for gzip silently parse to @[]@.
-- Pure, exported for tests.
decompressBody :: ResponseHeaders -> LBS.ByteString -> LBS.ByteString
decompressBody hs body =
  case lookup hContentEncoding hs of
    Just enc
      | enc == BSC.pack "gzip",    isGzip  body -> GZip.decompress body
      | enc == BSC.pack "deflate", isZlib  body -> Zlib.decompress body
      | otherwise                                -> body
    Nothing -> body
  where
    isGzip b = LBS.take 2 b == LBS.pack [0x1f, 0x8b]
    isZlib b = case LBS.uncons b of
      Just (0x78, _) -> True
      _              -> False

-- | IO: GET a URL with extra request headers (e.g. a Cookie for cookie-auth
-- indexers, m23) and a timeout in microseconds. Returns the HTTP status
-- code and body (m67: transparently decompressed when the server sent a
-- gzip/deflate Content-Encoding); 'Left' only on transport errors.
-- Redirects are followed by http-client, so a cookie-less indexer that
-- redirects to login.php still ends at a non-200 or a login page --
-- callers treat non-200 as "not logged in".
fetchUrl :: [Header] -> Int -> String -> IO (Either String (Int, LBS.ByteString))
fetchUrl extraHeaders micros url = do
  manager <- newManager settings
  let req0 = parseRequest_ url
      req = if null extraHeaders then req0 else req0 { requestHeaders = extraHeaders ++ requestHeaders req0 }
  eres <- try (httpLbs req manager)
  case eres of
    Left (e :: SomeException) ->
      pure (Left ("request failed: " ++ show e))
    Right resp ->
      pure (Right (statusCode (responseStatus resp)
                  , decompressBody (responseHeaders resp) (responseBody resp)))
  where
    settings = tlsManagerSettings
      { managerResponseTimeout = responseTimeoutMicro micros
      }

-- | IO: GET a Jackett search URL (10s timeout), parse the Torznab XML.
jackettSearch :: String -> IO (Either String [Release])
jackettSearch url = do
  eres <- fetchUrl [] trackerTimeoutMicros url
  case eres of
    Left err -> pure (Left err)
    Right (code, body)
      | code /= 200 ->
          pure (Left ("tracker returned HTTP " ++ show code))
      | otherwise ->
          pure (Right (parseTorznabResults body))
