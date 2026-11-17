module FMS.Core.Router
  ( buildApplication
  , firstMatch
  , matchSegments
  , isOpenPath
  , openPaths
  , parseTokenHeader
  , tokenFromRequest
  , parseCorsOrigins
  , allowedOriginsHeader
  , securityHeaders
  , normalizeSegments
  ) where

import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO, writeTVar)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString as B
import Data.CaseInsensitive (original)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (intercalate)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (getCurrentTime)
import Network.HTTP.Types
  ( Header, Method, ResponseHeaders
  , status204, status400, status401, status403, status404, status429, status500, status503
  )
import Network.Socket (SockAddr (..), hostAddressToTuple)
import Network.Wai
  ( Application, Request, Response
  , mapResponseHeaders, pathInfo, remoteHost, requestHeaders, requestMethod
  , responseLBS
  )

import FMS.Core.Module (Module(..), Route(..), Segment(..))
import FMS.Core.Monad (AppError(..), AppEnv(..), AppM, getState, putState, runApp)
import FMS.Core.RateLimit
  ( Bucket, newBucket, tryTake, rateLimitBucketsKey, requestCounterKey, requestIdHex )
import FMS.Core.Types.Config (Config(..))
import FMS.Core.Types.ServerConfig (ServerConfig(..))
import FMS.Core.Types.User (User(..))

-- | Flatten every module's routes into one WAI 'Application'.
--
-- * Every request gets a request id (hex counter) and every response
--   carries an @X-Request-Id@ header (milestone 14a); error responses
--   additionally embed it in the JSON body as @requestId@, and handler
--   errors are logged with it.
-- * @OPTIONS@ requests get a 204 preflight response (not rate limited).
-- * Every response carries CORS headers computed from
--   'configCorsOrigins' + the request Origin (milestone 13b; @*@ = the old
--   permissive behaviour), plus security headers when
--   'configSecurityHeaders' is on.
-- * Milestone 14a: when @configRateLimit@ is set, non-OPTIONS requests are
--   rate limited per client IP with a token bucket BEFORE the auth gate;
--   denied requests get 429 @{\"error\":\"rate limited\",\"requestId\":...}@.
-- * When 'configRequireAuth' is on, any request whose path is not in
--   'openPaths' must carry a valid credential ('parseTokenHeader'), checked
--   against the auth module's state in the typed registry ("auth.tokens" /
--   "auth.users"); missing/invalid credentials get a 401.
-- * Otherwise the first route matching method + path segments wins; the
--   handler runs inside 'runApp', and 'AppError' maps to 400/401/404/429/500.
buildApplication :: AppEnv -> [Module] -> Application
buildApplication env modules req respond0 = do
  ereqId <- runApp env nextRequestId
  let cfg     = envConfig env
      reqId   = either (const (requestIdHex 0)) id ereqId
      origin  = lookup "origin" (requestHeaders req)
      -- milestone 13b: CORS allow-origin is computed from configCorsOrigins
      -- + the request Origin; security headers when configSecurityHeaders
      corsH   = allowedOriginsHeader (configCorsOrigins cfg) (TE.decodeUtf8 <$> origin)
                ++ corsBaseHeaders
      secH    = if configSecurityHeaders cfg then securityHeaders else []
      respond resp = respond0 (withRequestId reqId (mapResponseHeaders ((secH ++ corsH) ++) resp))
  if requestMethod req == "OPTIONS"
    then respond (mapResponseHeaders (("Access-Control-Max-Age", "600") :) (responseLBS status204 [] ""))
    else do
      elim <- runApp env (rateLimitExceeded req)
      if elim == Right True
        then respond (errorResponse reqId (TooManyRequests "rate limited"))
        else do
          let routes = concatMap moduleRoutes modules
              -- milestone 17a: jellyfin-web calls PascalCase paths
              -- (/Items, /Videos/...); literal segments now compare
              -- case-insensitively in 'matchSegments' and 'isOpenPath'
              -- lowercases internally, so raw segments are passed through
              -- (captures keep their original case; no two routes differ
              -- only by case -- verified by route audit)
              segs   = pathInfo req
              dispatch =
                case firstMatch routes (requestMethod req) segs of
                  Nothing -> respond (errorResponse reqId (NotFound "404 Not Found"))
                  Just (route, captures) -> do
                    eres <- runApp env (routeHandler route captures req)
                    case eres of
                      Left err ->
                        putStrLn ("[ERROR] request " ++ T.unpack reqId ++ ": " ++ show err)
                      Right _ -> pure ()
                    respond (either (errorResponse reqId) id eres)
          ereq <- runApp env requireAuthNow
          let requireAuth = either (const (configRequireAuth (envConfig env))) id ereq
          if requireAuth && not (isOpenPath segs)
            then do
              eok <- runApp env (tokenIsValid req)
              if eok == Right True
                then dispatch
                else respond (unauthorizedResponse reqId)
            else dispatch

-- | Mint the next request id: a global counter TVar in the registry,
-- rendered as 16-char hex ('requestIdHex'). The TVar is created on first
-- use so tests without the full server env still get distinct ids.
nextRequestId :: AppM Text
nextRequestId = do
  mTv <- getState requestCounterKey :: AppM (Maybe (TVar Int))
  tv <- case mTv of
    Just t  -> pure t
    Nothing -> do
      t <- liftIO (newTVarIO 0)
      putState requestCounterKey t
      pure t
  n <- liftIO $ atomically $ do
    modifyTVar' tv (+ 1)
    readTVar tv
  pure (requestIdHex n)

-- | Add the @X-Request-Id@ header to a response (milestone 14a).
withRequestId :: Text -> Response -> Response
withRequestId reqId = mapResponseHeaders (("X-Request-Id", TE.encodeUtf8 reqId) :)

-- | Milestone 14a: has this client IP exhausted its token bucket?
-- Disabled (configRateLimit = Nothing or <= 0) or missing bucket TVar ->
-- never limited.
rateLimitExceeded :: Request -> AppM Bool
rateLimitExceeded req = do
  cfg <- asks envConfig
  case configRateLimit cfg of
    Nothing   -> pure False
    Just n | n <= 0 -> pure False
    Just n -> do
      let rate = fromIntegral n :: Double
      mTv <- getState rateLimitBucketsKey :: AppM (Maybe (TVar (HashMap Text Bucket)))
      case mTv of
        Nothing -> pure False
        Just tv -> do
          now <- liftIO getCurrentTime
          let ip = clientIpText req
          denied <- liftIO $ atomically $ do
            m <- readTVar tv
            let b          = HM.lookupDefault (newBucket rate now) ip m
                (ok, b')   = tryTake rate now b
            writeTVar tv (HM.insert ip b' m)
            pure (not ok)
          pure denied

-- | The client IP as text (SockAddrInet -> dotted quad; unix sockets ->
-- the path). The port is deliberately dropped so all connections from one
-- IP share a bucket.
clientIpText :: Request -> Text
clientIpText req =
  case remoteHost req of
    SockAddrInet _ h ->
      let (a, b, c, d) = hostAddressToTuple h
      in T.pack (intercalate "." (map show [a, b, c, d]))
    other            -> T.pack (show other)

-- | The live require-auth flag (milestone 1b): read the server-config TVar
-- from the typed registry so @POST /System/Configuration@ can flip it at
-- runtime; falls back to the static env config when the TVar is absent.
requireAuthNow :: AppM Bool
requireAuthNow = do
  mTv <- getState "system.serverConfig" :: AppM (Maybe (TVar ServerConfig))
  case mTv of
    Just tv -> liftIO (scRequireAuth <$> readTVarIO tv)
    Nothing -> asks (configRequireAuth . envConfig)

-- | Paths that never require a credential, even when 'configRequireAuth'
-- is on: server info, the health probe, and the two endpoints a client
-- needs in order to log in / discover users.
openPaths :: [[Text]]
openPaths =
  [ ["System", "Info", "Public"]
  , ["healthz"]
  , ["Users", "AuthenticateByName"]
  , ["Users", "Public"]
  -- milestone 10b: the first-run wizard must be reachable before any user
  -- exists (strict mode), and branding configuration is public like Jellyfin
  , ["System", "Wizard", "Startup"]
  , ["System", "Wizard", "Completed"]
  , ["Branding", "Configuration"]
  ]

-- | Is this concrete request path in the open list? Pure, exported for
-- tests. Milestone 17a: compared case-insensitively (see 'normalizeSegments').
isOpenPath :: [Text] -> Bool
isOpenPath segs = normalizeSegments segs `elem` map normalizeSegments openPaths

-- | Milestone 17a: case-fold path segments (jellyfin-web uses PascalCase
-- paths like /Items while the routes are registered lowercase).
-- Pure, exported for tests.
normalizeSegments :: [Text] -> [Text]
normalizeSegments = map T.toLower

-- | Extract a credential token from request headers:
-- @X-Emby-Token: \<t\>@, or @Authorization: MediaBrowser Token=\"\<t\>\"@,
-- or @Authorization: Bearer \<t\>@. Header names compare case-insensitively.
-- Pure, exported for tests.
parseTokenHeader :: [(B.ByteString, B.ByteString)] -> Maybe Text
parseTokenHeader hs =
  case findHeader "x-emby-token" of
    Just v | not (B.null v) -> Just (TE.decodeUtf8 v)
    _ -> case findHeader "authorization" of
      Just v  -> parseAuthorization (TE.decodeUtf8 v)
      Nothing -> Nothing
  where
    findHeader name =
      listToMaybe [v | (k, v) <- hs, T.toLower (TE.decodeUtf8 k) == name]

-- | The credential carried by a request, if any: 'parseTokenHeader' over
-- the raw (unwrapped) header list.
tokenFromRequest :: Request -> Maybe Text
tokenFromRequest req = parseTokenHeader (rawHeaders req)

-- | WAI header names are @CI ByteString@; unwrap them to plain
-- 'B.ByteString' so 'parseTokenHeader' stays a pure function on a plain
-- association list (and is trivially testable).
rawHeaders :: Request -> [(B.ByteString, B.ByteString)]
rawHeaders req = [(original k, v) | (k, v) <- requestHeaders req]

-- | Parse the value of an @Authorization@ header.
parseAuthorization :: Text -> Maybe Text
parseAuthorization txt
  | "mediabrowser" `T.isPrefixOf` lowered = tokenParam txt
  | "bearer" `T.isPrefixOf` lowered =
      let rest = T.strip (T.drop (T.length "bearer") txt)
      in if T.null rest then Nothing else Just rest
  | otherwise = Nothing
  where
    lowered = T.toLower txt

-- | Find @Token=\"...\"@ (key matched case-insensitively) and return the raw
-- token value, taken from the original (non-lowered) header text.
tokenParam :: Text -> Maybe Text
tokenParam txt =
  let low    = T.toLower txt
      needle = "token=\""
  in case T.breakOn needle low of
       (pre, rest)
         | T.null rest -> Nothing
         | otherwise ->
             let start = T.length pre + T.length needle
                 tok   = T.takeWhile (/= '"') (T.drop start txt)
             in if T.null tok then Nothing else Just tok

-- | Check the request's credential against the auth module's state in the
-- typed registry. The router knows only the key names ("auth.tokens" /
-- "auth.users") and the shape of the values — never the module itself. A
-- token is valid when it is a live session token OR an API key of some
-- user.
tokenIsValid :: Request -> AppM Bool
tokenIsValid req =
  case tokenFromRequest req of
    Nothing -> pure False
    Just tok -> do
      mTokens <- getState "auth.tokens" :: AppM (Maybe (TVar (HashMap Text Text)))
      mUsers  <- getState "auth.users"  :: AppM (Maybe (TVar (HashMap Text User)))
      case (mTokens, mUsers) of
        (Just tt, Just ut) -> liftIO $ do
          tokens <- readTVarIO tt
          users  <- readTVarIO ut
          -- milestone 9: a credential owned by a disabled user is invalid
          pure (case HM.lookup tok tokens of
                  Just uid -> maybe False userEnabled (HM.lookup uid users)
                  Nothing  -> any (\u -> userEnabled u && tok `elem` apiKeys u)
                                (HM.elems users))
        _ -> pure False

-- | 401 for the auth gate (missing/invalid credential on a protected
-- path). Milestone 14a: the body carries the request id.
unauthorizedResponse :: Text -> Response
unauthorizedResponse reqId =
  responseLBS status401 jsonContentType
    (encode (object ["error" .= ("unauthorized" :: Text), "requestId" .= reqId]))

-- | First route matching method + path segments; captures bound as pairs.
firstMatch :: [Route] -> Method -> [Text] -> Maybe (Route, [(Text, Text)])
firstMatch routes meth segs =
  listToMaybe
    [ (r, caps)
    | r <- routes
    , routeMethod r == meth
    , Just caps <- [matchSegments (routePattern r) segs]
    ]

-- | Match a route pattern against concrete path segments, collecting the
-- captures. Literal segments compare case-insensitively (milestone 17a);
-- capture segments bind the ORIGINAL segment (ids keep their case).
matchSegments :: [Segment] -> [Text] -> Maybe [(Text, Text)]
matchSegments [] [] = Just []
matchSegments (Lit l : rest) (s : segs)
  | T.toLower l == T.toLower s = matchSegments rest segs
matchSegments (Capture name : rest) (s : segs) =
  ((name, s) :) <$> matchSegments rest segs
matchSegments _ _ = Nothing

-- | Milestone 13b, pure: parse the FMS_CORS_ORIGINS env value
-- (comma-separated, whitespace trimmed, empties dropped). An empty/unset
-- value yields @[\"*\"]@ (allow all, the pre-13b behaviour).
parseCorsOrigins :: Text -> [Text]
parseCorsOrigins raw =
  let ts = [ T.strip t | t <- T.splitOn "," raw, not (T.null (T.strip t)) ]
  in if null ts then ["*"] else ts

-- | Milestone 13b, pure: the @Access-Control-Allow-Origin@ header(s) for a
-- request with the given @Origin@. When @\"*\"@ is configured -> @*@@
-- (regardless of whether the request carries Origin). Otherwise the origin
-- is reflected (plus @Vary: Origin@) only when it matches the configured
-- list case-insensitively; a restricted config with a non-matching (or
-- absent) Origin yields NO ACAO header at all, so browsers block the
-- cross-origin response.
allowedOriginsHeader :: [Text] -> Maybe Text -> [Header]
allowedOriginsHeader origins mOrigin
  | "*" `elem` map T.toCaseFold origins = [("Access-Control-Allow-Origin", "*")]
  | otherwise = case mOrigin of
      Just o | any (\c -> T.toCaseFold c == T.toCaseFold o) origins ->
        [ ("Access-Control-Allow-Origin", TE.encodeUtf8 o)
        , ("Vary", "Origin")
        ]
      _ -> []

-- | The always-on CORS base headers (methods/headers), as before 13b.
corsBaseHeaders :: [Header]
corsBaseHeaders =
  [ ("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
  , ("Access-Control-Allow-Headers", "*")
  ]

-- | Milestone 13b, pure: security headers added to every response when
-- configSecurityHeaders is on. NOTE: no Content-Security-Policy on
-- purpose — the built-in web client (FMS.Modules.Web) uses inline
-- scripts/styles, and a restrictive CSP would break it.
securityHeaders :: [Header]
securityHeaders =
  [ ("X-Content-Type-Options", "nosniff")
  , ("X-Frame-Options", "SAMEORIGIN")
  , ("Referrer-Policy", "no-referrer")
  ]

-- | Map an 'AppError' to its HTTP response. Milestone 14a: the body is
-- JSON @{\"error\": msg, \"requestId\": id}@ so clients can correlate
-- failures with server logs.
errorResponse :: Text -> AppError -> Response
errorResponse reqId err =
  let (st, msg) = case err of
        BadRequest     m -> (status400, m)
        Unauthorized   m -> (status401, m)
        Forbidden      m -> (status403, m)
        NotFound       m -> (status404, m)
        TooManyRequests m -> (status429, m)
        ServiceUnavailable m -> (status503, m)  -- milestone 15
        InternalError  m -> (status500, m)
  in responseLBS st jsonContentType
       (encode (object ["error" .= msg, "requestId" .= reqId]))

jsonContentType :: ResponseHeaders
jsonContentType = [("Content-Type", "application/json")]
