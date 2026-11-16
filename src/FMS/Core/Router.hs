module FMS.Core.Router
  ( buildApplication
  , firstMatch
  , matchSegments
  , isOpenPath
  , openPaths
  , parseTokenHeader
  , tokenFromRequest
  ) where

import Control.Concurrent.STM (TVar, readTVarIO)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode, object, (.=))
import qualified Data.ByteString as B
import Data.CaseInsensitive (original)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Network.HTTP.Types
  ( Method, ResponseHeaders
  , status204, status400, status401, status404, status500
  )
import Network.Wai
  ( Application, Request, Response
  , mapResponseHeaders, pathInfo, requestHeaders, requestMethod, responseLBS
  )

import FMS.Core.Module (Module(..), Route(..), Segment(..))
import FMS.Core.Monad (AppError(..), AppEnv(..), AppM, getState, runApp)
import FMS.Core.Types.Config (Config(..))
import FMS.Core.Types.User (User(..))

-- | Flatten every module's routes into one WAI 'Application'.
--
-- * @OPTIONS@ requests get a 204 preflight response.
-- * Every response carries permissive CORS headers (the server is called
--   from a UI on another origin).
-- * When 'configRequireAuth' is on, any request whose path is not in
--   'openPaths' must carry a valid credential ('parseTokenHeader'), checked
--   against the auth module's state in the typed registry ("auth.tokens" /
--   "auth.users"); missing/invalid credentials get a 401.
-- * Otherwise the first route matching method + path segments wins; the
--   handler runs inside 'runApp', and 'AppError' maps to 400/401/404/500.
buildApplication :: AppEnv -> [Module] -> Application
buildApplication env modules req respond
  | requestMethod req == "OPTIONS" =
      respond (cors (responseLBS status204 [] ""))
  | otherwise =
      let routes = concatMap moduleRoutes modules
          segs   = pathInfo req
          dispatch =
            case firstMatch routes (requestMethod req) segs of
              Nothing -> respond (cors (responseLBS status404 textPlain "404 Not Found"))
              Just (route, captures) -> do
                eres <- runApp env (routeHandler route captures req)
                respond (cors (either errorResponse id eres))
      in if configRequireAuth (envConfig env) && not (isOpenPath segs)
           then do
             eok <- runApp env (tokenIsValid req)
             if eok == Right True
               then dispatch
               else respond (cors unauthorizedResponse)
           else dispatch

-- | Paths that never require a credential, even when 'configRequireAuth'
-- is on: server info, the health probe, and the two endpoints a client
-- needs in order to log in / discover users.
openPaths :: [[Text]]
openPaths =
  [ ["System", "Info", "Public"]
  , ["healthz"]
  , ["Users", "AuthenticateByName"]
  , ["Users", "Public"]
  ]

-- | Is this concrete request path in the open list? Pure, exported for
-- tests.
isOpenPath :: [Text] -> Bool
isOpenPath segs = segs `elem` openPaths

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
          pure (HM.member tok tokens
                || any (\u -> tok `elem` apiKeys u) (HM.elems users))
        _ -> pure False

-- | 401 for the auth gate (missing/invalid credential on a protected path).
unauthorizedResponse :: Response
unauthorizedResponse =
  responseLBS status401 jsonContentType
    (encode (object ["error" .= ("unauthorized" :: Text)]))

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
-- captures. Literal segments must compare equal; capture segments bind.
matchSegments :: [Segment] -> [Text] -> Maybe [(Text, Text)]
matchSegments [] [] = Just []
matchSegments (Lit l : rest) (s : segs)
  | l == s = matchSegments rest segs
matchSegments (Capture name : rest) (s : segs) =
  ((name, s) :) <$> matchSegments rest segs
matchSegments _ _ = Nothing

-- | Permissive CORS headers on every response.
cors :: Response -> Response
cors = mapResponseHeaders (corsHeaders ++)
  where
    corsHeaders =
      [ ("Access-Control-Allow-Origin", "*")
      , ("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
      , ("Access-Control-Allow-Headers", "*")
      ]

errorResponse :: AppError -> Response
errorResponse err = case err of
  BadRequest    msg -> responseLBS status400 textPlain (TLE.encodeUtf8 (TL.fromStrict msg))
  Unauthorized  msg -> responseLBS status401 textPlain (TLE.encodeUtf8 (TL.fromStrict msg))
  NotFound      msg -> responseLBS status404 textPlain (TLE.encodeUtf8 (TL.fromStrict msg))
  InternalError msg -> responseLBS status500 textPlain (TLE.encodeUtf8 (TL.fromStrict msg))

textPlain :: ResponseHeaders
textPlain = [("Content-Type", "text/plain; charset=utf-8")]

jsonContentType :: ResponseHeaders
jsonContentType = [("Content-Type", "application/json")]
