module FMS.Modules.ApiDocs
  ( apiDocsModule
  , segmentToPathPiece
  , routeToPath
  , methodText
  , routesToOpenApi
  , apiDocsPageHtml
  ) where

import Data.Aeson (Value, encode, object, (.=))
import qualified Data.Aeson.Key as Key
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types (Method, status200)
import Network.Wai (Request, Response, responseLBS)

import FMS (version)
import FMS.Core.Module (Module (..), Route (..), Segment (..), get)
import FMS.Core.Monad (AppM, getState)

-- | The API-docs module (docs/todo.org section 1, milestone 1c): serves a
-- GENERATED OpenAPI 3.0.3 spec at @/api-docs/openapi.json@, built from the
-- actual route table of every loaded module (read from the typed registry
-- under "system.modules", which "FMS.Server" registers), plus a tiny HTML
-- index page at @/api-docs@.
apiDocsModule :: Module
apiDocsModule = Module
  { moduleName   = "apidocs"
  , moduleInit   = pure ()
  , moduleRoutes =
      [ get [Lit "api-docs", Lit "openapi.json"] openApiHandler
      , get [Lit "api-docs"]                      apiDocsPageHandler
      ]
  }

-- | @GET /api-docs/openapi.json@ — 200 application/json with the spec
-- generated from the live module list (empty paths when the registry has
-- no modules, e.g. in tests).
openApiHandler :: [(Text, Text)] -> Request -> AppM Response
openApiHandler _captures _req = do
  mMods <- getState "system.modules" :: AppM (Maybe [Module])
  let mods  = fromMaybe [] mMods
      spec  = routesToOpenApi [ (moduleName m, moduleRoutes m) | m <- mods ]
  pure (responseLBS status200
    [("Content-Type", "application/json")] (encode spec))

-- | @GET /api-docs@ — tiny ASCII HTML index linking to the spec.
apiDocsPageHandler :: [(Text, Text)] -> Request -> AppM Response
apiDocsPageHandler _captures _req =
  pure (responseLBS status200
    [("Content-Type", "text/html; charset=utf-8")] apiDocsPageHtml)

-- Pure OpenAPI generators (exported for tests) ------------------------------

-- | One route-pattern segment as an OpenAPI path piece:
-- @Lit \"items\"@ -> @items@, @Capture \"id\"@ -> @{id}@.
segmentToPathPiece :: Segment -> Text
segmentToPathPiece (Lit t)    = t
segmentToPathPiece (Capture n) = "{" <> n <> "}"

-- | The OpenAPI path for a route: @Lit "items", Capture "id"@ ->
-- @/items/{id}@; the empty pattern (root) -> @/@.
routeToPath :: Route -> Text
routeToPath r = "/" <> T.intercalate "/" (map segmentToPathPiece (routePattern r))

-- | HTTP method as lowercase text (@\"get\"@, @\"post\"@, ...).
methodText :: Method -> Text
methodText = T.toLower . TE.decodeUtf8

-- | Build an OpenAPI 3.0.3 document from @(moduleName, routes)@ pairs.
-- Paths are grouped across modules; each operation carries the module
-- name as its tag, the route summary/description (empty unless the route
-- was declared with getDoc/postDoc/deleteDoc), path parameters for every
-- capture, and a single documented @200@ response.
routesToOpenApi :: [(Text, [Route])] -> Value
routesToOpenApi modRoutes = object
  [ "openapi" .= ("3.0.3" :: Text)
  , "info"    .= object
      [ "title"   .= ("functional-media-server" :: Text)
      , "version" .= T.pack version
      ]
  , "paths"   .= object
      [ (Key.fromText path, object [ (Key.fromText (methodText (routeMethod r)), operation modName r)
                                   | (modName, r) <- grouped ])
      | (path, grouped) <- Map.toList byPath
      ]
  ]
  where
    allTagged = [ (mn, r) | (mn, rs) <- modRoutes, r <- rs ]
    byPath   = Map.fromListWith (++) [ (routeToPath r, [(mn, r)]) | (mn, r) <- allTagged ]

    operation modName r = object
      [ "summary"     .= routeSummary r
      , "description" .= routeDescription r
      , "tags"        .= [modName]
      , "parameters"  .= [ param n | Capture n <- routePattern r ]
      , "responses"   .= object
          [ "200" .= object [ "description" .= ("OK" :: Text) ] ]
      ]
    param n = object
      [ "name"     .= n
      , "in"       .= ("path" :: Text)
      , "required" .= True
      , "schema"   .= object [ "type" .= ("string" :: Text) ]
      ]

-- | The tiny HTML index page (ASCII only).
apiDocsPageHtml :: LBS.ByteString
apiDocsPageHtml = LBSC.pack (unlines
  [ "<!DOCTYPE html>"
  , "<html><head><meta charset='utf-8'><title>functional-media-server API docs</title></head>"
  , "<body style='font-family:sans-serif'>"
  , "<h1>functional-media-server API docs</h1>"
  , "<p>Generated OpenAPI 3.0.3 spec (built from the live module route table):"
  , " <a href='/api-docs/openapi.json'>/api-docs/openapi.json</a></p>"
  , "</body></html>"
  ])
