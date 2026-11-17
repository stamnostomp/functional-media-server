module FMS.Modules.Web
  ( webModule
  , webPageHtml
  , safeUiPath
  , contentTypeFor
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Char (toLower)
import Data.List (isPrefixOf)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types (status200)
import Network.Wai (Request, Response, responseLBS)
import System.Directory (doesFileExist)
import System.FilePath (isAbsolute, normalise, splitDirectories, takeExtension, (</>))

import FMS.Core.Module (Module (..), Segment (..), get)
import FMS.Core.Monad (AppError (..), AppM, envConfig)
import FMS.Core.Types.Config (Config (..))

-- | The web module: a single-page client served at @/@ and @/web@.
-- Without 'configUiDir' that is the built-in minimal vanilla-JS page
-- (docs/todo.org section 13, milestone 13/16). When @FMS_UI_DIR@ /
-- @--ui-dir@ points at a built stamno-media UI (elm.js + index.html +
-- css), @/@ and @/web@ serve its @index.html@ and the single-segment
-- capture route serves the other files from the same directory
-- (same-origin: the UI calls the API at localhost:8080 = our default
-- port). webModule stays AFTER all API modules in the module list
-- ("FMS.Server"), so single-segment literal routes (/library, /healthz,
-- ...) win over the capture.
webModule :: Module
webModule = Module
  { moduleName   = "web"
  , moduleInit   = pure ()
  , moduleRoutes =
      [ get []               webHandler
      , get [Lit "web"]      webHandler
        -- capture LAST: only reached when no literal route matched
      , get [Capture "file"] uiFileHandler
      ]
  }

-- | Both the root @/@ and @/web@ serve the single-page client: the
-- stamno-media @index.html@ when 'configUiDir' is set, else the built-in
-- inline page.
webHandler :: [(Text, Text)] -> Request -> AppM Response
webHandler _captures _req = do
  cfg <- asks envConfig
  case configUiDir cfg of
    Just uiDir ->
      case safeUiPath uiDir "index.html" of
        Just p -> serveUiFile p
        Nothing -> throwError (InternalError "uiDir/index.html rejected by safeUiPath")
    Nothing ->
      pure (responseLBS status200
        [("Content-Type", "text/html; charset=utf-8")] webPageHtml)

-- | @GET /<file>@ (single segment): serve @<uiDir>\/<file>@ when
-- 'configUiDir' is set, the path passes 'safeUiPath' and the file
-- exists; otherwise 404 (the normal JSON error body). Without
-- 'configUiDir' every such path 404s (behavior unchanged).
uiFileHandler :: [(Text, Text)] -> Request -> AppM Response
uiFileHandler captures _req = do
  cfg <- asks envConfig
  case configUiDir cfg of
    Nothing -> throwError (NotFound "404 Not Found")
    Just uiDir ->
      case lookup "file" captures of
        Nothing -> throwError (NotFound "404 Not Found")
        Just f ->
          case safeUiPath uiDir (T.unpack f) of
            Nothing -> throwError (NotFound "404 Not Found")
            Just p -> do
              exists <- liftIO (doesFileExist p)
              if exists then serveUiFile p
                        else throwError (NotFound "404 Not Found")

-- | Read and serve one UI file with its extension's Content-Type.
serveUiFile :: FilePath -> AppM Response
serveUiFile p = do
  bytes <- liftIO (LBS.readFile p)
  pure (responseLBS status200
    [("Content-Type", contentTypeFor p)] bytes)

-- | Pure: resolve @rel@ under @uiDir@, rejecting anything that could
-- escape it: null bytes, absolute paths, and any @..@ path segment. The
-- normalised result must stay under the normalised @uiDir@. 'Nothing'
-- means reject (the caller answers 404). Existence is NOT checked here
-- (that is IO); the handler does 'doesFileExist'.
safeUiPath :: FilePath -> FilePath -> Maybe FilePath
safeUiPath uiDir rel
  | '\0' `elem` rel                    = Nothing
  | isAbsolute rel                     = Nothing
  | any (== "..") (splitDirectories rel) = Nothing
  | otherwise =
      let uiNorm = normalise uiDir
          joined = normalise (uiDir </> rel)
      in if uiNorm == joined || (uiNorm ++ "/") `isPrefixOf` joined
           then Just joined
           else Nothing

-- | Pure: the Content-Type for a UI file by extension (lowercased);
-- unknown extensions get @application\/octet-stream@.
contentTypeFor :: FilePath -> BSC.ByteString
contentTypeFor p = BSC.pack $ case map toLower (takeExtension p) of
  ".html" -> "text/html; charset=utf-8"
  ".js"   -> "application/javascript"
  ".css"  -> "text/css; charset=utf-8"
  ".png"  -> "image/png"
  ".jpg"  -> "image/jpeg"
  ".jpeg" -> "image/jpeg"
  ".gif"  -> "image/gif"
  ".svg"  -> "image/svg+xml"
  ".json" -> "application/json"
  ".ico"  -> "image/x-icon"
  ".map"  -> "application/json"
  ".woff" -> "font/woff"
  ".woff2" -> "font/woff2"
  _       -> "application/octet-stream"

-- | The whole page as one inline lazy ByteString (ASCII only, vanilla JS,
-- no external assets). Exported so the test suite can assert on its
-- markers. Note: every HTML/JS attribute uses single quotes so the Haskell
-- string literals need no escaping.
webPageHtml :: LBS.ByteString
webPageHtml = LBSC.pack (unlines
  [ "<!DOCTYPE html>"
  , "<html><head><meta charset='utf-8'>"
  , "<meta name='viewport' content='width=device-width, initial-scale=1'>"
  , "<title>functional-media-server</title>"
  , "<style>"
  , "body{font-family:sans-serif;background:#101010;color:#eee;margin:1em}"
  , "h1{font-size:1.3em}"
  , "#q{width:100%;box-sizing:border-box;padding:.5em;margin-bottom:1em;background:#222;color:#eee;border:1px solid #444;border-radius:4px}"
  , "#grid{display:flex;flex-wrap:wrap;gap:1em}"
  , ".card{background:#1c1c1c;padding:.5em;width:180px;border-radius:6px}"
  , ".card img{width:180px;height:100px;object-fit:cover;background:#000;border-radius:4px}"
  , ".card .t{font-weight:bold;margin:.3em 0}"
  , ".card .s{color:#999;font-size:.8em}"
  , ".card a{color:#4af;margin-right:.5em;font-size:.85em}"
  , "#player{margin:1em 0;min-height:2em}"
  , "#player video{max-width:100%;max-height:60vh;background:#000}"
  , "</style></head><body>"
  , "<h1>functional-media-server</h1>"
  , "<input id='q' placeholder='Search items...' oninput='load(this.value)'>"
  , "<div id='player'></div>"
  , "<div id='grid'></div>"
  , "<script>"
  , "function esc(s){var d=document.createElement('div');d.textContent=String(s);return d.innerHTML;}"
  , "async function load(q){"
  , "  var r=await fetch('/items?search='+encodeURIComponent(q||''));"
  , "  var d=await r.json();"
  , "  var g=document.getElementById('grid');"
  , "  g.innerHTML='';"
  , "  (d.Items||[]).forEach(function(it){"
  , "    var c=document.createElement('div'); c.className='card';"
  , "    var img=document.createElement('img');"
  , "    img.src='/items/'+it.Id+'/thumb'; img.alt=it.Name;"
  , "    c.appendChild(img);"
  , "    var t=document.createElement('div'); t.className='t'; t.textContent=it.Name;"
  , "    c.appendChild(t);"
  , "    var s=document.createElement('div'); s.className='s'; s.textContent=it.Size+' bytes';"
  , "    c.appendChild(s);"
  , "    var links=document.createElement('div');"
  , "    var play=document.createElement('a'); play.href='#'; play.textContent='Play';"
  , "    play.onclick=function(){showPlayer(it.Id);return false;};"
  , "    var hls=document.createElement('a'); hls.href='/videos/'+it.Id+'/main.m3u8'; hls.textContent='HLS';"
  , "    var meta=document.createElement('a'); meta.href='/items/'+it.Id+'/metadata'; meta.textContent='Meta';"
  , "    links.appendChild(play); links.appendChild(hls); links.appendChild(meta);"
  , "    c.appendChild(links);"
  , "    g.appendChild(c);"
  , "  });"
  , "}"
  , "function showPlayer(id){"
  , "  document.getElementById('player').innerHTML='<video controls autoplay src=/stream/'+esc(id)+'></video>';"
  , "}"
  , "load('');"
  , "</script></body></html>"
  ])
