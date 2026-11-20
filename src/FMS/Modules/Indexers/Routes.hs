module FMS.Modules.Indexers.Routes
  ( indexersRoutes
  , CreateIndexer (..)
  , fetchUrlBytes
  , resolveReleaseBytes
  ) where

import Control.Concurrent.Async (mapConcurrently)
import Control.Exception (SomeException, try)
import Control.Monad (unless, when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (FromJSON (..), eitherDecode, encode, object, withObject, (.:), (.:?), (.!=), (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import Data.List (nub, partition)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client (managerResponseTimeout, requestHeaders, responseBody, responseStatus, responseTimeoutMicro)
import Network.HTTP.Conduit (httpLbs, newManager, parseRequest_)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (HeaderName, ResponseHeaders, hCookie, status200, status201, status204, statusCode)
import Network.Wai (Response, queryString, responseLBS, strictRequestBody)
import qualified Network.Wai as Wai

import FMS.Core.Module (Route, Segment (..), delete, get, jsonResponse, post)
import FMS.Core.Monad (AppError (..), AppM, envConfig)
import FMS.Core.Query (queryParams)
import FMS.Core.Types.Config (Config (..))
import FMS.Core.Types.User (User (..))
import FMS.Modules.Auth.Monad (currentUserFromReq, requireAdmin)
import FMS.Modules.Indexers.Monad (MonadIndexers (..), newIndexerId)
import FMS.Modules.Metadata.Api.Tmdb (tmdbSearchMovieAt, tmdbSearchTvAt)
import FMS.Modules.Metadata.Api.Types (TmdbResult (..), originalTitlesOf)
import FMS.Modules.Metadata.Monad (getMetadataSettings)
import FMS.Modules.Metadata.Types (msTmdbApiKey)
import FMS.Modules.Indexers.Types
  ( Indexer (..)
  , base64UrlDecode
  , base64UrlEncode
  , defaultCategories
  , defaultSearchPath
  , hostMatches
  , indexerCookieHeader
  , indexerFullJson
  , indexerPublicJson
  , indexerSearchUrl
  , indexerSearchUrlExtra
  , parseTorrentDayJson
  , torrentDaySearchUrl
  , validAuthType
  , validResponseFormat
  )
import FMS.Modules.Requests.Push (slugify)
import FMS.Modules.Trackers.Torznab (fetchUrl, parseTorznabResults)
import FMS.Modules.Trackers.Types (Release (..), dedupeReleases, sortReleasesBySeeders)

jsonCT :: ResponseHeaders
jsonCT = [("Content-Type", "application/json")]

-- | The direct-indexer API (m21). Reads (list/search/download) are open to
-- any authenticated user but REDACT the ApiKey for non-admins and NEVER
-- expose raw indexer URLs (downloads are proxied); mutations are
-- admin-only. Literals before captures (first-match-wins).
indexersRoutes :: [Route]
indexersRoutes =
  [ get    [Lit "Indexers"] listIndexersHandler
  , post   [Lit "Indexers"] createIndexerHandler
  , get    [Lit "Indexers", Lit "Search"] searchHandler
  , get    [Lit "Indexers", Capture "id"] getIndexerHandler
  , delete [Lit "Indexers", Capture "id"] deleteIndexerHandler
  , post   [Lit "Indexers", Capture "id", Lit "Enable"] (setEnabledHandler True)
  , post   [Lit "Indexers", Capture "id", Lit "Disable"] (setEnabledHandler False)
  , post   [Lit "Indexers", Capture "id", Lit "Test"] testIndexerHandler
  , get    [Lit "Indexers", Capture "id", Lit "Download"] downloadHandler
  ]

-- | Body of @POST /Indexers@ (admin); m23 adds AuthType/Cookie/
-- ResponseFormat/SearchPath.
data CreateIndexer = CreateIndexer
  { ciName           :: Text
  , ciBaseUrl        :: Text
  , ciApiKey         :: Text
  , ciCategories     :: [Int]
  , ciType           :: Text
  , ciEnabled        :: Bool
  , ciAuthType       :: Text
  , ciCookie         :: Text
  , ciResponseFormat :: Text
  , ciSearchPath     :: Text
  }

instance FromJSON CreateIndexer where
  parseJSON = withObject "CreateIndexer" $ \o ->
    CreateIndexer
      <$> o .:  "Name"
      <*> o .:  "BaseUrl"
      <*> o .:? "ApiKey" .!= ""
      <*> o .:? "Categories" .!= defaultCategories
      <*> o .:? "Type" .!= "torznab"
      <*> o .:? "Enabled" .!= True
      <*> o .:? "AuthType" .!= "apikey"
      <*> o .:? "Cookie" .!= ""
      <*> o .:? "ResponseFormat" .!= "torznab"
      <*> o .:? "SearchPath" .!= defaultSearchPath

-- Handlers ------------------------------------------------------------------

isAdminReq :: Wai.Request -> AppM Bool
isAdminReq req = do
  mUser <- currentUserFromReq req
  pure (maybe False isAdmin mUser)

-- | @GET /Indexers@ — any authenticated user; ApiKey redacted (null) for
-- non-admins.
listIndexersHandler :: [(Text, Text)] -> Wai.Request -> AppM Response
listIndexersHandler _captures req = do
  admin <- isAdminReq req
  is <- listIndexers
  pure (jsonResponse (map (indexerPublicJson admin) is))

-- | @GET /Indexers/:id@ — same redaction rule; 404 unknown.
getIndexerHandler :: [(Text, Text)] -> Wai.Request -> AppM Response
getIndexerHandler captures req = do
  admin <- isAdminReq req
  iid <- captureId captures
  m <- getIndexer iid
  case m of
    Nothing -> throwError (NotFound ("no indexer with id " <> iid))
    Just i  -> pure (jsonResponse (indexerPublicJson admin i))

-- | @POST /Indexers@ (admin) — 201 with the created indexer (full JSON,
-- the creator is admin). BaseUrl must be http(s); Type must be
-- torznab|newznab.
createIndexerHandler :: [(Text, Text)] -> Wai.Request -> AppM Response
createIndexerHandler _captures req = do
  _ <- requireAdmin req
  body <- liftIO (strictRequestBody req)
  ci <- case eitherDecode body of
    Left err -> throwError (BadRequest ("invalid JSON body: " <> T.pack err))
    Right v  -> pure v
  let u = T.toLower (ciBaseUrl ci)
  unless (T.isPrefixOf "http://" u || T.isPrefixOf "https://" u)
    (throwError (BadRequest "BaseUrl must start with http:// or https://"))
  let ty = T.toLower (ciType ci)
  unless (ty == "torznab" || ty == "newznab")
    (throwError (BadRequest "Type must be torznab or newznab"))
  let at = T.toLower (ciAuthType ci)
  unless (validAuthType at)
    (throwError (BadRequest "AuthType must be apikey or cookie"))
  let rf = T.toLower (ciResponseFormat ci)
  unless (validResponseFormat rf)
    (throwError (BadRequest "ResponseFormat must be torznab or torrentday-json"))
  when (at == "cookie" && T.null (T.strip (ciCookie ci)))
    (throwError (BadRequest "Cookie is required when AuthType is cookie"))
  iid <- liftIO newIndexerId
  let i = Indexer { indexerId = iid
                  , indexerName = ciName ci
                  , indexerBaseUrl = ciBaseUrl ci
                  , indexerApiKey = ciApiKey ci
                  , indexerCategories = ciCategories ci
                  , indexerType = ty
                  , indexerEnabled = ciEnabled ci
                  , indexerLastTestAt = Nothing
                  , indexerLastTestOk = Nothing
                  , indexerAuthType = at
                  , indexerCookie = ciCookie ci
                  , indexerResponseFormat = rf
                  , indexerSearchPath = ciSearchPath ci }
  _ <- insertIndexer i
  pure (responseLBS status201 jsonCT (encode (indexerFullJson i)))

-- | @DELETE /Indexers/:id@ (admin) — 204, 404 unknown.
deleteIndexerHandler :: [(Text, Text)] -> Wai.Request -> AppM Response
deleteIndexerHandler captures req = do
  _ <- requireAdmin req
  iid <- captureId captures
  ok <- deleteIndexer iid
  if not ok
    then throwError (NotFound ("no indexer with id " <> iid))
    else pure (responseLBS status204 [] "")

-- | @POST /Indexers/:id/Enable|Disable@ (admin) — 200 with the updated
-- indexer (full JSON; caller is admin), 404 unknown.
setEnabledHandler :: Bool -> [(Text, Text)] -> Wai.Request -> AppM Response
setEnabledHandler enabled captures req = do
  _ <- requireAdmin req
  iid <- captureId captures
  ok <- setIndexerEnabled iid enabled
  if not ok
    then throwError (NotFound ("no indexer with id " <> iid))
    else do
      m <- getIndexer iid
      case m of
        Just i  -> pure (jsonResponse (indexerFullJson i))
        Nothing -> throwError (NotFound ("no indexer with id " <> iid))

-- | @GET /Indexers/Search?q=&type=movie|tv&indexerId=&season=&ep=@ — any
-- authenticated user. @season@/@ep@ (m60) are forwarded to torznab
-- indexers for episode / season-pack searches. Queries all enabled
-- indexers (or just @indexerId@ when given) in
-- parallel, tolerating per-indexer failures (recorded in "Errors").
-- Results carry a PROXIED DownloadUrl (@/Indexers/<id>/Download?r=..@),
-- never the raw indexer link (which embeds the apikey).
-- m66: @q@ may be REPEATED — every non-blank @q@ is searched and the
-- results are merged (deduped by title, still sorted by seeders). The
-- stamno-media Discover flow passes the English display title AND the
-- TMDB original title (OriginalName), so original-language releases
-- (e.g. "El laberinto del fauno" for Pan's Labyrinth) show up too.
-- Additionally, when a TMDB API key is configured, each query is expanded
-- SERVER-SIDE with the top TMDB hit's original title ('originalTitlesOf'),
-- so even a single-@q@ client (the /dashboard search box, the legacy
-- /Trackers/:id/Search callers) gets original-language results.
searchHandler :: [(Text, Text)] -> Wai.Request -> AppM Response
searchHandler _captures req = do
  let qparams = queryParams (queryString req)
      -- m66: all q params, blanks dropped, exact duplicates removed
      -- (order kept, so the English title stays the primary query)
      qs0 = nub [ v | ("q", v) <- qparams, not (T.null (T.strip v)) ]
  case qs0 of
    [] -> throwError (BadRequest "missing q parameter")
    _  -> pure ()
  -- m66: server-side original-language expansion via TMDB (graceful: no
  -- key or TMDB failure -> just the client-supplied queries)
  cfg <- asks envConfig
  ms <- getMetadataSettings
  extraQs <- if T.null (msTmdbApiKey ms)
    then pure []
    else liftIO (originalLanguageQueries (configTmdbBaseUrl cfg) (msTmdbApiKey ms) qs0)
  let qs = nub (qs0 ++ extraQs)
  let mType = lookup "type" qparams
      mIndexerId = lookup "indexerid" qparams
      -- m60: optional torznab season/ep params (episode / season-pack
      -- searches from the Discover tab); only well-formed ints pass through
      extras =
        [ (k, v) | k <- ["season", "ep"], Just v <- [lookup k qparams]
                 , maybe False (const True) (readMaybe (T.unpack v) :: Maybe Int) ]
  allIndexers <- listIndexers
  selected <- case mIndexerId of
    Just iid ->
      case [ i | i <- allIndexers, indexerId i == iid ] of
        [] -> throwError (NotFound ("no indexer with id " <> iid))
        (i:_) -> do
          when (not (indexerEnabled i)) (throwError (BadRequest "indexer is disabled"))
          pure [i]
    Nothing -> pure (filter indexerEnabled allIndexers)
  -- m66: one (indexer, query) search per pair, all in parallel
  perQuery <- liftIO (mapConcurrently
    (\(i, q) -> do
      eres <- searchIndexer i mType q extras
      pure (i, eres))
    [ (i, q) | i <- selected, q <- qs ])
  let (okPairs, errPairs) = partition (\(_, e) -> either (const False) (const True) e) perQuery
      -- m66: flatten + dedupe (same release can match several queries),
      -- then sort by seeders so the [RECOMMENDED] row is still the best
      results = [ releaseJson i r
                | (i, r) <- sortReleasesBySeeders
                    (dedupeReleases [ (i, r) | (i, Right rels) <- okPairs, r <- rels ]) ]
      -- one error per indexer (first failure wins) -- the object would
      -- collapse duplicate keys anyway, but be explicit
      errors = nub [ (indexerId i, T.pack msg) | (i, Left msg) <- errPairs ]
  pure (jsonResponse (object
    [ "Results" .= results
    , "Errors"  .= object [ (Key.fromText iid, Aeson.String msg) | (iid, msg) <- errors ]
    ]))

-- | One search result in API JSON. The raw download URL is replaced by a
-- proxied path; ReleaseId is its base64url token (decodable only
-- server-side). No apikey-bearing URL is ever emitted.
releaseJson :: Indexer -> Release -> Aeson.Value
releaseJson i r =
  object (base ++ dl)
  where
    base =
      [ "Title"       .= relTitle r
      , "SizeBytes"   .= relSizeBytes r
      , "Seeders"     .= relSeeders r
      , "IndexerId"   .= indexerId i
      , "IndexerName" .= indexerName i
      , "Magnet"      .= relMagnet r
      ]
    dl = case relDownloadUrl r of
      Nothing -> []
      Just raw ->
        let rid = base64UrlEncode raw
        in [ "ReleaseId" .= rid
           , "DownloadUrl" .= ("/Indexers/" <> indexerId i <> "/Download?r=" <> rid)
           ]

-- | @GET /Indexers/:id/Download?r=<releaseId>@ — any authenticated user.
-- Decodes the releaseId back to the raw indexer URL, enforces the SSRF
-- guard (raw host must match the indexer base host, else 400), fetches
-- with a 15s timeout and streams the .torrent back as an attachment.
downloadHandler :: [(Text, Text)] -> Wai.Request -> AppM Response
downloadHandler captures req = do
  iid <- captureId captures
  mIdx <- getIndexer iid
  case mIdx of
    Nothing -> throwError (NotFound ("no indexer with id " <> iid))
    Just _ -> pure ()
  let qparams = queryParams (queryString req)
  r <- case lookup "r" qparams of
    Just v | not (T.null v) -> pure v
    _ -> throwError (BadRequest "missing r parameter")
  eres <- resolveReleaseBytes iid r
  case eres of
    Left err -> throwError (BadRequest (T.pack err))
    Right (raw, bytes) ->
      let fname = slugFor raw
      in pure (responseLBS status200
            [ ("Content-Type", "application/x-bittorrent")
            , ("Content-Disposition", BSC.pack ("attachment; filename=" ++ fname ++ ".torrent"))
            ]
            (LBS.fromStrict bytes))

-- | Resolve a proxied release token to the raw indexer URL + fetched
-- .torrent bytes. Shared by the HTTP download route and by the requests
-- push pipeline (m21 fix: the push must not HTTP-fetch its own auth-gated
-- proxy URL in strict mode). Returns the raw URL alongside the bytes for
-- the filename. AppM errors are returned as Left strings so IO callers
-- (pushRequest) can consume this via runApp.
resolveReleaseBytes :: Text -> Text -> AppM (Either String (Text, BS.ByteString))
resolveReleaseBytes iid r = do
  m <- getIndexer iid
  case m of
    Nothing -> pure (Left ("no indexer with id " ++ T.unpack iid))
    Just idx -> do
      raw <- case base64UrlDecode r of
        Just u | T.isPrefixOf "http://" (T.toLower u) || T.isPrefixOf "https://" (T.toLower u) -> pure u
        _ -> pure ""
      if T.null raw
        then pure (Left "invalid r parameter")
        else if not (hostMatches raw (indexerBaseUrl idx))
          then pure (Left "download url host does not match indexer base url")
          else do
            eres <- liftIO (fetchUrlBytes (cookieHeaders idx) 15000000 (T.unpack raw))
            pure (fmap (\b -> (raw, b)) eres)

-- | @POST /Indexers/:id/Test@ (admin) — run a q=test search against the
-- indexer, persist LastTestAt/LastTestOk, respond {"Ok": bool, "Error"?}. -}
testIndexerHandler :: [(Text, Text)] -> Wai.Request -> AppM Response
testIndexerHandler captures req = do
  _ <- requireAdmin req
  iid <- captureId captures
  m <- getIndexer iid
  case m of
    Nothing -> throwError (NotFound ("no indexer with id " <> iid))
    Just idx -> do
      eres <- liftIO (searchIndexer idx (Just "movie") "test" [])
      let (ok, mErr) = case eres of
            Right _   -> (True, Nothing)
            Left msg  -> (False, Just (T.pack msg))
      _ <- setIndexerTestResult iid ok
      pure (jsonResponse (object $
        [ "Ok" .= ok ] ++ [ "Error" .= e | Just e <- [mErr] ]))

-- Internals -----------------------------------------------------------------

captureId :: [(Text, Text)] -> AppM Text
captureId captures =
  case lookup "id" captures of
    Just i  -> pure i
    Nothing -> throwError (BadRequest "missing id capture")

-- | A safe attachment filename derived from the raw URL's last path
-- segment (ASCII slug), falling back to "release".
slugFor :: Text -> String
slugFor raw =
  let segs = filter (not . T.null . T.strip) (T.splitOn "/" (T.takeWhile (/= '?') raw))
      seg = case reverse segs of
        []    -> ""
        (x:_) -> x
      s = slugify seg
  in if null s || s == "request" then "release" else s

-- | GET a URL with extra headers (e.g. the indexer Cookie for cookie-auth
-- proxied downloads, m23) and a timeout (microseconds), returning the raw
-- body bytes on HTTP 200. Used for the proxied .torrent download.
fetchUrlBytes :: [(HeaderName, BS.ByteString)] -> Int -> String -> IO (Either String BS.ByteString)
fetchUrlBytes extraHeaders micros url = do
  manager <- newManager (tlsManagerSettings
    { managerResponseTimeout = responseTimeoutMicro micros })
  let req0 = parseRequest_ url
      req = if null extraHeaders then req0 else req0 { requestHeaders = extraHeaders ++ requestHeaders req0 }
  eres <- try (httpLbs req manager)
  case eres of
    Left (e :: SomeException) ->
      pure (Left ("fetch failed: " ++ show e))
    Right resp
      | statusCode (responseStatus resp) == 200 ->
          pure (Right (LBS.toStrict (responseBody resp)))
      | otherwise ->
          pure (Left ("fetch returned HTTP " ++ show (statusCode (responseStatus resp))))

-- | Cookie request header for cookie-auth indexers (m23), as raw header
-- pairs for 'fetchUrlBytes'.
cookieHeaders :: Indexer -> [(HeaderName, BS.ByteString)]
cookieHeaders idx =
  [ (hCookie, TE.encodeUtf8 c) | ("Cookie", c) <- indexerCookieHeader idx ]

-- | m66: for each query, ask TMDB (top movie hit + top tv hit) and
-- collect original titles that are not already among the queries, so a
-- single English query also matches original-language releases (Pan's
-- Labyrinth -> "El laberinto del fauno"). Never throws: TMDB transport /
-- parse failures yield no extra queries.
originalLanguageQueries :: Text -> Text -> [Text] -> IO [Text]
originalLanguageQueries base apiKey qs = do
  perQ <- mapConcurrently
    (\q -> do
      em <- tmdbSearchMovieAt base apiKey q
      es <- tmdbSearchTvAt base apiKey q
      let top r = case either (const []) id r of
            (x : _) -> [x]
            []      -> []
      pure (top em ++ top es))
    qs
  pure (originalTitlesOf qs (concat perQ))

-- | Search one indexer (m23: format- and auth-aware). Torznab/Newznab
-- goes to @<searchPath>?t=search...@ and is parsed as XML; TorrentDay
-- t.json goes to @/t.json?<cats>;q=...@ and is parsed as JSON. Cookie
-- auth sends the Cookie header (and omits the apikey param). A non-200
-- response on a cookie indexer is reported as "not logged in".
searchIndexer :: Indexer -> Maybe Text -> Text -> [(Text, Text)] -> IO (Either String [Release])
searchIndexer idx mType q extras = do
  let headers = [ (hCookie, TE.encodeUtf8 c) | ("Cookie", c) <- indexerCookieHeader idx ]
      isJson = indexerResponseFormat idx == "torrentday-json"
      url = if isJson
              then torrentDaySearchUrl idx q
              else indexerSearchUrlExtra idx mType q extras
  eres <- fetchUrl headers 10000000 url
  case eres of
    Left err -> pure (Left err)
    Right (code, body)
      | code /= 200 ->
          pure (Left (if indexerAuthType idx == "cookie"
                         then "not logged in (cookie missing/expired)"
                         else "indexer returned HTTP " ++ show code))
      | isJson -> pure (Right (parseTorrentDayJson (indexerBaseUrl idx) body))
      | otherwise -> pure (Right (parseTorznabResults body))
