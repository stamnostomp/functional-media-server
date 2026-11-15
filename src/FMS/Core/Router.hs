module FMS.Core.Router
  ( buildApplication
  , firstMatch
  , matchSegments
  ) where

import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Network.HTTP.Types
  ( Method, ResponseHeaders
  , status204, status400, status404, status500
  )
import Network.Wai
  ( Application, Response
  , mapResponseHeaders, pathInfo, requestMethod, responseLBS
  )

import FMS.Core.Module (Module(..), Route(..), Segment(..))
import FMS.Core.Monad (AppError(..), AppEnv, runApp)

-- | Flatten every module's routes into one WAI 'Application'.
--
-- * @OPTIONS@ requests get a 204 preflight response.
-- * Every response carries permissive CORS headers (the server is called
--   from a UI on another origin).
-- * Otherwise the first route matching method + path segments wins; the
--   handler runs inside 'runApp', and 'AppError' maps to 400/404/500.
buildApplication :: AppEnv -> [Module] -> Application
buildApplication env modules req respond
  | requestMethod req == "OPTIONS" =
      respond (cors (responseLBS status204 [] ""))
  | otherwise =
      let routes = concatMap moduleRoutes modules
          segs   = pathInfo req
      in case firstMatch routes (requestMethod req) segs of
           Nothing -> respond (cors (responseLBS status404 textPlain "404 Not Found"))
           Just (route, captures) -> do
             eres <- runApp env (routeHandler route captures req)
             respond (cors (either errorResponse id eres))

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
  NotFound      msg -> responseLBS status404 textPlain (TLE.encodeUtf8 (TL.fromStrict msg))
  InternalError msg -> responseLBS status500 textPlain (TLE.encodeUtf8 (TL.fromStrict msg))

textPlain :: ResponseHeaders
textPlain = [("Content-Type", "text/plain; charset=utf-8")]
