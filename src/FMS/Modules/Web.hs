module FMS.Modules.Web
  ( webModule
  , webPageHtml
  , dashboardPageHtml
  , dashboardBeforeCapture
  , safeUiPath
  , contentTypeFor
  , pageAuthOk
  , redirectToRoot
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Char (toLower)
import Data.List (isPrefixOf)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types (status200, status302)
import Network.Wai (Request, Response, responseLBS)
import System.Directory (doesFileExist)
import System.FilePath (isAbsolute, normalise, splitDirectories, takeExtension, (</>))

import FMS.Core.Module (Module (..), Route (..), Segment (..), get)
import FMS.Core.Monad (AppError (..), AppM, envConfig)
import FMS.Core.Types.Config (Config (..))
import FMS.Modules.Auth.Monad (currentUserFromReq)

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
      [ get []                  webHandler
      , get [Lit "web"]         webHandler
      , get [Lit "dashboard"]   dashboardHandler
        -- capture LAST: only reached when no literal route matched
      , get [Capture "file"]    uiFileHandler
      ]
  }

-- | Pure: is the @Lit \"dashboard\"@ route registered before the
-- single-segment @Capture \"file\"@ static route? First match wins, so
-- @/dashboard@ must precede the capture even when 'configUiDir' is set
-- (exported for tests).
dashboardBeforeCapture :: [Route] -> Bool
dashboardBeforeCapture rs =
  case [ i | (i, r) <- zip [0 :: Int ..] rs, routePattern r == [Lit "dashboard"] ] of
    (i : _) -> any (\(j, r) -> j > i && routePattern r == [Capture "file"]) (zip [0 ..] rs)
    []      -> False

-- | m58: in auth mode a browser page may only be served to a signed-in
-- user; anonymous visitors get a 302 to @/@ (the single-page client,
-- whose login screen is the one sign-in page). In open mode (no
-- FMS_REQUIRE_AUTH) every page is served. Used by the HTML page
-- handlers (/dashboard, /api-docs); API routes keep their 401 JSON.
pageAuthOk :: Request -> AppM Bool
pageAuthOk req = do
  cfg <- asks envConfig
  if not (configRequireAuth cfg)
    then pure True
    else do
      mUser <- currentUserFromReq req
      pure (isJust mUser)

-- | 302 to the single-page client at @/@ (the sign-in page).
redirectToRoot :: Response
redirectToRoot =
  responseLBS status302 [("Location", "/")] LBS.empty

-- | @GET /dashboard@ -- the built-in admin dashboard page
-- (milestone 19b), served regardless of 'configUiDir' (the Lit route
-- wins over the static capture, see 'dashboardBeforeCapture').
-- m58: anonymous visitors in auth mode are redirected to @/@.
dashboardHandler :: [(Text, Text)] -> Request -> AppM Response
dashboardHandler _captures req = do
  ok <- pageAuthOk req
  if ok
    then pure (responseLBS status200
           [("Content-Type", "text/html; charset=utf-8")] dashboardPageHtml)
    else pure redirectToRoot

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

-- | The whole admin dashboard page (milestone 19b) as one inline lazy
-- ByteString: ASCII only, vanilla JS, no external assets, single-quoted
-- HTML/JS attributes (no escaping in the Haskell literals). The page
-- logs in via POST /Users/AuthenticateByName when the API answers 401,
-- stores the AccessToken in localStorage and sends it as the
-- X-Emby-Token header on every later fetch; it auto-refreshes every 10s
-- and shows Server / Memory / Library / Sessions / Activity / Scheduled
-- tasks / Plugins / Requests sections plus Rescan and Refresh-metadata
-- buttons (m20c: Requests queue with Approve/Reject for pending items).
-- m22: the page is a tabbed app -- OVERVIEW keeps the sections above, the
-- TRACKERS tab (admin-only, muted note for non-admins; admin probe is
-- GET /Users, 403 for non-admins) holds the Jackett-style indexer manager
-- (GET/POST /Indexers, Enable/Disable/Test/Delete), a search-all box
-- (GET /Indexers/Search) with a REQUEST button per release (POST
-- /Requests, 409 -> ALREADY IN PROGRESS), and the m20c external-Jackett
-- /Trackers CRUD moved into a subsection. The 10s auto-refresh only
-- renders the visible tab. m24: the add-indexer form also posts AuthType
-- (apikey|cookie, toggling the api-key vs cookie-password inputs),
-- ResponseFormat (torznab|torrentday-json) and SearchPath, and the
-- indexers table shows Auth/Format columns.
-- Exported so the test suite can assert on its markers.
dashboardPageHtml :: LBS.ByteString
dashboardPageHtml = LBSC.pack (unlines
  [ "<!DOCTYPE html>"
  , "<html><head><meta charset='utf-8'>"
  , "<meta name='viewport' content='width=device-width, initial-scale=1'>"
  , "<title>/dashboard</title>"
  , "<style>"
  , ":root{"
  , "--black: #000000; --void: #0a0b0d; --gray-1: #1a1a1a; --gray-2: #333333; --gray-3: #666666;"
  , "--gray-4: #808080; --gray-5: #c0c0c0; --gray-6: #e8e8e8; --white: #ffffff;"
  , "}"
  , "*{border-radius: 0 !important; box-sizing: border-box;}"
  , "body{margin:0; background:var(--black); color:var(--white); font-family:'Courier New', Courier, monospace;}"
  , ".fg-primary{color:var(--white)} .fg-secondary{color:var(--gray-5)} .fg-muted{color:var(--gray-4)}"
  , ".bg-page{background:var(--black)} .bg-panel{background:var(--gray-1)}"
  , ".b--default{border:1px solid var(--gray-3)} .b--strong{border:1px solid var(--white)}"
  , ".tracked-wide{letter-spacing:0.1em} .tracked-wider{letter-spacing:0.2em}"
  , ".text-md{font-size:1.125rem} .text-xl{font-size:2rem}"
  , ".sticky-top{position:sticky; top:0; z-index:50;}"
  , "header{position:sticky; top:0; z-index:50; background:var(--black); border-bottom:1px solid var(--gray-3); display:flex; align-items:center; gap:24px; padding:12px 24px;}"
  , "header .brand{color:var(--white); text-decoration:none; font-size:1.125rem; letter-spacing:0.2em; text-transform:uppercase;}"
  , "header nav a{color:var(--gray-4); text-decoration:none; font-size:0.875rem; letter-spacing:0.1em; text-transform:uppercase; margin-right:16px;}"
  , "header nav a:hover{color:var(--white)}"
  , "header .spacer{flex:1}"
  , "main{padding:24px; max-width:1100px;}"
  , ".panel{background:var(--gray-1); border:1px solid var(--gray-3); padding:16px; margin-bottom:24px;}"
  , ".panel h2{margin:0 0 12px; font-size:0.875rem; font-weight:normal; letter-spacing:0.1em; text-transform:uppercase; color:var(--gray-5);}"
  , "table{border-collapse:collapse; width:100%;}"
  , "th{color:var(--gray-4); font-weight:normal; text-align:left; font-size:0.75rem; letter-spacing:0.1em; text-transform:uppercase; padding:6px 12px 6px 0; border-bottom:1px solid var(--gray-2);}"
  , "td{color:var(--gray-6); font-size:0.875rem; padding:6px 12px 6px 0; border-bottom:1px solid var(--gray-2); vertical-align:top;}"
  , "tbody tr:hover, table tr:hover{background:rgba(255,255,255,0.05);}"
  , "ul{margin:0; padding-left:16px;} li{font-size:0.875rem; color:var(--gray-5); padding:2px 0;}"
  , ".btn{display:inline-flex; align-items:center; justify-content:center; height:44px; padding:0 24px; cursor:pointer; font-family:inherit; font-size:0.875rem; letter-spacing:0.1em; text-transform:uppercase; border-radius:0; margin-right:8px;}"
  , ".btn-primary{background:var(--white); color:var(--black); border:1px solid var(--white);}"
  , ".btn-primary:hover{background:var(--gray-6);}"
  , ".btn-ghost{background:transparent; color:var(--white); border:1px solid var(--gray-3);}"
  , ".btn-ghost:hover{border-color:var(--white);}"
  , "button{background:transparent; color:var(--white); border:1px solid var(--gray-3); cursor:pointer; font-family:inherit; font-size:0.75rem; letter-spacing:0.1em; text-transform:uppercase; padding:6px 12px; border-radius:0;}"
  , "button:hover{border-color:var(--white);}"
  , "input{background:transparent; border:1px solid var(--gray-3); color:var(--white); font-family:inherit; font-size:0.875rem; letter-spacing:0.1em; padding:10px 12px; outline:none; border-radius:0; margin-right:8px;}"
  , "input:focus{border-color:var(--white);}"
  , "input::placeholder{color:var(--gray-4); text-transform:uppercase; letter-spacing:0.1em;}"
  , "#err{color:var(--white); min-height:1.2em; font-size:0.875rem; letter-spacing:0.1em; text-transform:uppercase;}"
  , "#login{padding:48px 24px;}"
  , "#login form{padding:24px; margin:0 auto; max-width:560px; display:flex; gap:8px;}"
  , "</style></head><body class='bg-page'>"
  , "<header class='sticky-top'>"
  , "  <a class='brand' href='/dashboard'>/dashboard</a>"
  , "  <nav>"
  , "    <a href='/'>web ui</a>"
  , "    <a href='/api-docs'>api-docs</a>"
  , "  </nav>"
  , "  <span class='spacer'></span>"
  , "  <span id='err' class='fg-muted tracked-wide'></span>"
  , "</header>"
  , "<div id='tabbar' class='flex gap-3 bb b--default' style='padding:12px 24px; overflow-x:auto'>"
  , "  <button id='tabbtn-overview' class='btn btn-primary' style='white-space:nowrap' onclick=\"setTab('overview')\">OVERVIEW</button>"
  , "  <button id='tabbtn-trackers' class='btn btn-ghost' style='white-space:nowrap' onclick=\"setTab('trackers')\">TRACKERS</button>"
  , "  <button id='tabbtn-requests' class='btn btn-ghost' style='white-space:nowrap' onclick=\"setTab('requests')\">REQUESTS</button>"
  , "  <button id='tabbtn-users' class='btn btn-ghost' style='white-space:nowrap' onclick=\"setTab('users')\">USERS</button>"
  , "</div>"
  , "<main>"
  , "<div id='login' style='display:none'>"
  , "  <form onsubmit='return doLogin()' class='panel bg-panel b--default'>"
  , "    <input id='lu' placeholder='username' autocomplete='username'>"
  , "    <input id='lp' type='password' placeholder='password' autocomplete='current-password'>"
  , "    <button type='submit' class='btn btn-primary' style='margin-right:0'>Sign in</button>"
  , "  </form>"
  , "</div>"
  , "<div id='tab-overview'>"
  , "<section class='panel bg-panel b--default'>"
  , "  <h2>Actions</h2>"
  , "  <button class='btn btn-primary' onclick='rescan()'>Rescan library</button>"
  , "  <button class='btn btn-ghost' onclick='refreshMeta()'>Refresh metadata</button>"
  , "  <button class='btn btn-ghost' onclick='genIntro()'>Generate skip intro</button>"
  , "  <span id='intronote' class='f7 fg-muted ttu tracked-wide' style='margin-left:12px'></span>"
  , "</section>"
  , "<section class='panel bg-panel b--default'><h2>Server</h2><div id='server'>...</div></section>"
  , "<section class='panel bg-panel b--default'><h2>Memory</h2><div id='mem'>...</div></section>"
  , "<section class='panel bg-panel b--default'><h2>Library</h2><div id='library'>...</div></section>"
  , "<section class='panel bg-panel b--default'><h2>Sessions</h2><div id='sessions'>...</div></section>"
  , "<section class='panel bg-panel b--default'><h2>Activity</h2><div id='activity'>...</div></section>"
  , "<section class='panel bg-panel b--default'><h2>Scheduled tasks</h2><div id='tasks'>...</div></section>"
  , "<section class='panel bg-panel b--default'><h2>Plugins</h2><div id='plugins'>...</div></section>"
  , "</div>"
  , "<div id='tab-trackers' style='display:none'>"
  , "<section class='panel bg-panel b--default'><h2>Indexers</h2><div id='indexers'>...</div></section>"
  , "<section class='panel bg-panel b--default'><h2>Search all indexers</h2>"
  , "  <div class='flex items-center gap-3 mb3'>"
  , "    <input id='sq' placeholder='QUERY$'>"
  , "    <select id='st' class='bg-panel ba b--default fg-primary f6 tracked-wide' style='padding:8px 12px'><option value='movie'>movie</option><option value='tv'>tv</option></select>"
  , "    <button class='btn btn-primary' onclick='searchAll()'>Search</button>"
  , "  </div>"
  , "  <div id='reqnote' class='f7 fg-muted ttu tracked-wide mb2'></div>"
  , "  <div id='searchres'>...</div>"
  , "</section>"
  , "<section class='panel bg-panel b--default' id='trackers-sec'><h2>External Jackett servers</h2><div id='trackers'>...</div></section>"
  , "<section class='panel bg-panel b--default' id='dlsec'><h2>Download client</h2><div id='dlcfg'>...</div><div id='dlstatus' class='f7 fg-muted ttu tracked-wide' style='margin-top:8px'></div></section>"
  , "<section class='panel bg-panel b--default' id='metasec'><h2>Metadata (TMDB)</h2><div id='metacfg'>...</div><div id='metastatus' class='f7 fg-muted ttu tracked-wide' style='margin-top:8px'></div></section>"
  , "</div>"
  , "<div id='tab-requests' style='display:none'>"
  , "<section class='panel bg-panel b--default' id='requests-sec'><h2>Requests</h2><div id='requests'>...</div></section>"
  , "</div>"
  , "<div id='tab-users' style='display:none'>"
  , "<section class='panel bg-panel b--default' id='users-sec'><h2>Users</h2><div id='users'>...</div></section>"
  , "<section class='panel bg-panel b--default'><h2>Create user</h2>"
  , "  <div class='flex items-center gap-3 flex-wrap'>"
  , "    <input id='nu' placeholder='username' autocomplete='username'>"
  , "    <input id='np' type='password' placeholder='password' autocomplete='new-password'>"
  , "    <label class='f7 fg-secondary ttu tracked-wide'><input id='na' type='checkbox'> admin</label>"
  , "    <button class='btn btn-primary' onclick='createUser()'>Create</button>"
  , "  </div>"
  , "  <div id='usernote' class='f7 fg-muted ttu tracked-wide' style='margin-top:8px'></div>"
  , "</section>"
  , "</div>"
  , "</main>"
  , "<script>"
  , "var TOKEN = localStorage.getItem('fmsToken') || '';"
  , "var IS_ADMIN = false;"
  , "var currentTab = 'overview';"
  , "var LAST_RESULTS = [];"
  , "var DL_LOADED = false;"
  , "var META_LOADED = false;"
  , "function el(id){ return document.getElementById(id); }"
  , "function esc(s){ var d=document.createElement('div'); d.textContent=String(s==null?'':s); return d.innerHTML; }"
  , "function fmtBytes(b){ b=Number(b)||0; if(b>=1073741824) return (b/1073741824).toFixed(1)+' GB'; if(b>=1048576) return (b/1048576).toFixed(1)+' MB'; if(b>=1024) return (b/1024).toFixed(1)+' KB'; return b+' B'; }"
  , "function fmtUp(s){ s=Number(s)||0; var d=Math.floor(s/86400), h=Math.floor(s%86400/3600), m=Math.floor(s%3600/60); return (d?d+'d ':'')+(h?h+'h ':'')+m+'m'; }"
  , "function row(k,v){ return '<tr><th>'+esc(k)+'</th><td>'+esc(v)+'</td></tr>'; }"
  , "function api(path, opts){"
  , "  opts = opts || {};"
  , "  opts.headers = { 'X-Emby-Token': TOKEN };"
  , "  if (opts.body) { opts.headers['Content-Type'] = 'application/json'; }"
  , "  return fetch(path, opts).then(function(r){"
  , "    if (r.status === 401) { el('login').style.display='block'; throw new Error('unauthorized'); }"
  , "    return r.json().catch(function(){ return {}; });"
  , "  });"
  , "}"
  , "function doLogin(){"
  , "  var u=el('lu').value, p=el('lp').value;"
  , "  fetch('/Users/AuthenticateByName', { method:'POST', headers:{'Content-Type':'application/json'}, body: JSON.stringify({Username:u, Pw:p}) })"
  , "    .then(function(r){ return r.json(); })"
  , "    .then(function(d){"
  , "      if (d.AccessToken) { TOKEN=d.AccessToken; localStorage.setItem('fmsToken', TOKEN); el('login').style.display='none'; el('err').textContent=''; refresh(); }"
  , "      else { el('err').textContent='login failed'; }"
  , "    })"
  , "    .catch(function(){ el('err').textContent='login failed'; });"
  , "  return false;"
  , "}"
  , "function section(id, html){"
  , "  var c = el(id); if (!c) { return; }"
  , "  var vals = {};"
  , "  var fields = c.querySelectorAll('input,select,textarea');"
  , "  for (var i = 0; i < fields.length; i++) { if (fields[i].id) { vals[fields[i].id] = fields[i].value; } }"
  , "  c.innerHTML = html;"
  , "  var ks = Object.keys(vals);"
  , "  for (var j = 0; j < ks.length; j++) { var f = el(ks[j]); if (f) { f.value = vals[ks[j]]; } }"
  , "  if (el('ia')) { toggleAuthFields(); }"
  , "}"
  , "function showErr(e){ el('err').textContent = String(e && e.message ? e.message : e); }"
  , "var TABS = ['overview','trackers','requests','users'];"
  , "function setTab(t){"
  , "  currentTab = t;"
  , "  TABS.forEach(function(x){"
  , "    el('tab-'+x).style.display = (x===t) ? '' : 'none';"
  , "    el('tabbtn-'+x).className = 'btn ' + (x===t ? 'btn-primary' : 'btn-ghost');"
  , "  });"
  , "  refresh();"
  , "}"
  , "function refresh(){"
  , "  fetch('/Users', {headers:{'X-Emby-Token':TOKEN}}).then(function(r){"
  , "    IS_ADMIN = (r.status===200);"
  , "    renderTab();"
  , "  }).catch(function(){"
  , "    IS_ADMIN=false;"
  , "    renderTab();"
  , "  });"
  , "}"
  , "function renderTab(){"
  , "  if (currentTab==='trackers') { renderTrackersTab(); }"
  , "  else if (currentTab==='requests') { renderRequests(); }"
  , "  else if (currentTab==='users') { renderUsersTab(); }"
  , "  else { renderOverview(); }"
  , "}"
  , "function renderUsersTab(){"
  , "  api('/Users').then(function(us){"
  , "    el('users').innerHTML = '<table><tr><th>Id</th><th>Name</th><th>Admin</th><th>Enabled</th><th>Has password</th><th></th></tr>'"
  , "      + (us||[]).map(function(u){ return '<tr><td>'+esc(u.Id)+'</td><td>'+esc(u.Name)+'</td><td>'+(u.IsAdministrator?'yes':'no')+'</td><td>'+(u.Enabled?'yes':'no')+'</td><td>'+(u.HasPassword?'yes':'no')+'</td><td><button class=&quot;btn btn-ghost&quot; data-uid=&quot;'+esc(u.Id)+'&quot; onclick=&quot;deleteUser(this.dataset.uid)&quot;&gt;Delete</button></td></tr>'; }).join('')"
  , "      + '</table>';"
  , "  }).catch(showErr);"
  , "}"
  , "function createUser(){"
  , "  var body = { Name: el('nu').value, Password: el('np').value, IsAdmin: el('na').checked };"
  , "  api('/Users', { method:'POST', body: JSON.stringify(body) }).then(function(d){"
  , "    el('usernote').textContent = d && d.Id ? ('CREATED ' + d.Name + ' (' + d.Id + ')') : 'CREATE FAILED';"
  , "    el('nu').value=''; el('np').value=''; el('na').checked=false;"
  , "    renderUsersTab();"
  , "  }).catch(function(e){ el('usernote').textContent = 'CREATE FAILED: ' + (e && e.message ? e.message : e); });"
  , "}"
  , "function deleteUser(id){"
  , "  if (!confirm('Delete user ' + id + '?')) { return; }"
  , "  api('/Users/'+encodeURIComponent(id), { method:'DELETE' }).then(function(){"
  , "    el('usernote').textContent = 'DELETED ' + id;"
  , "    renderUsersTab();"
  , "  }).catch(function(e){ el('usernote').textContent = 'DELETE FAILED: ' + (e && e.message ? e.message : e); });"
  , "}"
  , "function renderOverview(){"
  , "  api('/Dashboard').then(function(d){"
  , "    var s=d.Server||{}, l=d.Library||{};"
  , "    section('server', '<table>'"
  , "      + row('Name', s.ServerName)"
  , "      + row('Version', s.Version)"
  , "      + row('Port', s.Port)"
  , "      + row('Library path', s.LibraryPath)"
  , "      + row('Auth required', s.RequireAuth)"
  , "      + row('Uptime', fmtUp(s.UptimeSeconds))"
  , "      + row('Memory', fmtBytes(s.MemoryUsageBytes))"
  , "      + '</table>');"
  , "    var counts=l.Counts||{};"
  , "    var cs=Object.keys(counts).sort().map(function(k){ return k+': '+counts[k]; }).join(', ');"
  , "    section('library', '<table>' + row('Total items', l.TotalItems) + row('By type', cs||'-') + '</table>');"
  , "    var act=d.Activity||[];"
  , "    section('activity', act.length"
  , "      ? '<ul>'+act.slice(0,10).map(function(a){ return '<li>'+esc(a.Time)+' ['+esc(a.Type)+'] '+esc(a.Message)+'</li>'; }).join('')+'</ul>'"
  , "      : '<i>no activity</i>');"
  , "    var tasks=d.ScheduledTasks||[];"
  , "    section('tasks', tasks.length"
  , "      ? '<table><tr><th>Id</th><th>Name</th><th>Every(s)</th><th>Enabled</th><th>Last run</th><th>Last result</th><th></th></tr>'"
  , "        + tasks.map(function(t){ return '<tr><td>'+esc(t.Id)+'</td><td>'+esc(t.Name)+'</td><td>'+esc(t.IntervalSeconds)+'</td><td>'+esc(t.Enabled)+'</td><td>'+esc(t.LastRun||'')+'</td><td>'+esc(t.LastResult||'')+'</td><td><button data-id='+encodeURIComponent(t.Id)+' onclick=\"runTask(this)\">Run</button></td></tr>'; }).join('')"
  , "        + '</table>'"
  , "      : '<i>no scheduled tasks</i>');"
  , "  }).catch(showErr);"
  , "  api('/System/Memory').then(function(m){"
  , "    var n=(m.Samples||[]).length;"
  , "    section('mem', '<table>' + row('Current RSS', fmtBytes(m.CurrentRssBytes)) + row('Samples', n) + '</table>');"
  , "  }).catch(function(){ section('mem', '<i>unavailable (admin only)</i>'); });"
  , "  api('/Sessions').then(function(ss){"
  , "    section('sessions', ss.length"
  , "      ? '<table><tr><th>Id</th><th>User</th><th>Item</th><th>Position(s)</th><th>Paused</th><th>Client</th></tr>'"
  , "        + ss.map(function(x){ return '<tr><td>'+esc(x.Id)+'</td><td>'+esc(x.UserName||'')+'</td><td>'+esc(x.ItemId||'')+'</td><td>'+esc(x.PositionSeconds||0)+'</td><td>'+esc(x.IsPaused)+'</td><td>'+esc(x.Client||'')+'</td></tr>'; }).join('')"
  , "        + '</table>'"
  , "      : '<i>no active sessions</i>');"
  , "  }).catch(function(){ section('sessions', '<i>unavailable</i>'); });"
  , "  api('/Plugins').then(function(ps){"
  , "    section('plugins', ps.length"
  , "      ? '<table><tr><th>Id</th><th>Name</th><th>Version</th><th>Description</th><th>Enabled</th><th></th></tr>'"
  , "        + ps.map(function(p){ return '<tr><td>'+esc(p.Id)+'</td><td>'+esc(p.Name)+'</td><td>'+esc(p.Version)+'</td><td>'+esc(p.Description||'')+'</td><td>'+esc(p.Enabled)+'</td><td>'"
  , "          + (p.Enabled"
  , "            ? '<button data-id='+encodeURIComponent(p.Id)+' data-en=0 onclick=\"togglePlugin(this)\">Disable</button>'"
  , "            : '<button data-id='+encodeURIComponent(p.Id)+' data-en=1 onclick=\"togglePlugin(this)\">Enable</button>')"
  , "          + '</td></tr>'; }).join('')"
  , "        + '</table>'"
  , "      : '<i>no plugins</i>');"
  , "  }).catch(function(){ section('plugins', '<i>unavailable</i>'); });"
  , "}"
  , "function renderTrackersTab(){"
  , "  if (!IS_ADMIN) {"
  , "    section('indexers','<i>admin only</i>');"
  , "    section('searchres','<i>admin only</i>');"
  , "    section('trackers','<i>admin only</i>');"
  , "    return;"
  , "  }"
  , "  renderIndexers();"
  , "  renderTrackers();"
  , "  renderDlcfg();"
  , "  renderMeta();"
  , "}"
  , "function runTask(btn){ api('/ScheduledTasks/'+btn.getAttribute('data-id')+'/Run', {method:'POST'}).then(refresh).catch(showErr); }"
  , "function togglePlugin(btn){"
  , "  var id=btn.getAttribute('data-id'), en=btn.getAttribute('data-en')==='1';"
  , "  api('/Plugins/'+id+(en?'/Enable':'/Disable'), {method:'POST'}).then(refresh).catch(showErr);"
  , "}"
  , "function renderRequests(){"
  , "  api('/Requests').then(function(d){"
  , "    var rs=d.Requests||[];"
  , "    section('requests', rs.length"
  , "      ? '<table><tr><th>Title</th><th>User</th><th>Status</th><th>Size</th><th>Source</th><th>Dir</th><th>Created</th>'+(IS_ADMIN?'<th></th>':'')+'</tr>'"
  , "        + rs.map(function(r){"
  , "            var src=(r.SourceRelease||'')+(r.SourceIndexer?' ['+r.SourceIndexer+']':'');"
  , "            var btns=(IS_ADMIN && r.Status==='Pending') ? '<button data-id='+encodeURIComponent(r.Id)+' onclick=\"approveReq(this)\">Approve</button> <button data-id='+encodeURIComponent(r.Id)+' onclick=\"rejectReq(this)\">Reject</button>' : '';"
  , "            return '<tr><td>'+esc(r.Title)+(r.Year?' ('+esc(r.Year)+')':'')+'</td><td>'+esc(r.UserName||'')+'</td><td>'+esc(r.Status)+'</td><td>'+fmtBytes(r.SizeBytes)+'</td><td>'+esc(src||'-')+'</td><td>'+esc(r.DownloadDir||'-')+'</td><td>'+esc(String(r.CreatedAt||'').slice(0,19))+'</td>'+(IS_ADMIN?'<td>'+btns+'</td>':'')+'</tr>';"
  , "          }).join('')"
  , "        + '</table>'"
  , "      : '<i>no requests</i>');"
  , "  }).catch(function(){ section('requests','<i>unavailable</i>'); });"
  , "}"
  , "function renderTrackers(){"
  , "  api('/Trackers').then(function(ts){"
  , "    section('trackers', (ts.length"
  , "      ? '<table><tr><th>Id</th><th>Name</th><th>Url</th><th>Enabled</th><th></th></tr>'"
  , "        + ts.map(function(t){ return '<tr><td>'+esc(t.Id)+'</td><td>'+esc(t.Name)+'</td><td>'+esc(t.Url)+'</td><td>'+esc(t.Enabled)+'</td><td>'"
  , "          + (t.Enabled ? '<button data-id='+encodeURIComponent(t.Id)+' data-en=0 onclick=\"toggleTracker(this)\">Disable</button>' : '<button data-id='+encodeURIComponent(t.Id)+' data-en=1 onclick=\"toggleTracker(this)\">Enable</button>')"
  , "          + ' <button data-id='+encodeURIComponent(t.Id)+' onclick=\"deleteTracker(this)\">Delete</button></td></tr>'; }).join('')"
  , "        + '</table>'"
  , "      : '<i>no trackers</i>')"
  , "      + '<div style=\"margin-top:12px\"><input id=\"tn\" placeholder=\"name\"><input id=\"tu\" placeholder=\"url\"><input id=\"tk\" placeholder=\"api key\"><button class=\"btn btn-primary\" onclick=\"addTracker()\">Add tracker</button></div>');"
  , "  }).catch(function(){ section('trackers','<i>unavailable</i>'); });"
  , "}"
  , "function approveReq(b){ api('/Requests/'+b.getAttribute('data-id')+'/Approve',{method:'POST'}).then(refresh).catch(showErr); }"
  , "function rejectReq(b){ api('/Requests/'+b.getAttribute('data-id')+'/Reject',{method:'POST'}).then(refresh).catch(showErr); }"
  , "function toggleTracker(b){ var id=b.getAttribute('data-id'), en=b.getAttribute('data-en')==='1'; api('/Trackers/'+id+(en?'/Enable':'/Disable'),{method:'POST'}).then(refresh).catch(showErr); }"
  , "function deleteTracker(b){ api('/Trackers/'+b.getAttribute('data-id'),{method:'DELETE'}).then(refresh).catch(showErr); }"
  , "function addTracker(){ api('/Trackers',{method:'POST',body:JSON.stringify({Name:el('tn').value,Url:el('tu').value,ApiKey:el('tk').value})}).then(refresh).catch(showErr); }"
  , "function renderIndexers(){"
  , "  api('/Indexers').then(function(is){"
  , "    section('indexers', (is.length"
  , "      ? '<table><tr><th>Id</th><th>Name</th><th>BaseUrl</th><th>Type</th><th>Auth</th><th>Format</th><th>Cats</th><th>Enabled</th><th>Last test</th><th></th></tr>'"
  , "        + is.map(function(i){ return '<tr><td>'+esc(i.Id)+'</td><td>'+esc(i.Name)+'</td><td>'+esc(i.BaseUrl)+'</td><td>'+esc(i.Type)+'</td><td>'+esc(i.AuthType||'apikey')+'</td><td>'+esc(i.ResponseFormat||'torznab')+'</td><td>'+esc((i.Categories||[]).join(','))+'</td><td>'+esc(i.Enabled)+'</td><td>'+(i.LastTestOk==null?'':(i.LastTestOk?'ok':'FAIL'))+'</td><td>'"
  , "          + '<button data-id='+encodeURIComponent(i.Id)+' onclick=\"testIndexer(this)\">Test</button> '"
  , "          + (i.Enabled ? '<button data-id='+encodeURIComponent(i.Id)+' data-en=0 onclick=\"toggleIndexer(this)\">Disable</button>' : '<button data-id='+encodeURIComponent(i.Id)+' data-en=1 onclick=\"toggleIndexer(this)\">Enable</button>')"
  , "          + ' <button data-id='+encodeURIComponent(i.Id)+' onclick=\"deleteIndexer(this)\">Delete</button></td></tr>'; }).join('')"
  , "        + '</table>'"
  , "      : '<i>no indexers</i>')"
  , "      + '<div style=\"margin-top:12px\" class=\"flex items-center flex-wrap gap-2\">'"
  , "        + '<input id=\"in\" placeholder=\"name\">'"
  , "        + '<input id=\"ib\" placeholder=\"base url\">'"
  , "        + '<select id=\"ia\" class=\"bg-panel ba b--default fg-primary f6 tracked-wide\" style=\"padding:8px 12px\" onchange=\"toggleAuthFields()\">'"
  , "          + '<option value=\"apikey\">auth: apikey</option><option value=\"cookie\">auth: cookie</option></select>'"
  , "        + '<input id=\"ik\" placeholder=\"api key\">'"
  , "        + '<input id=\"ick\" type=\"password\" placeholder=\"cookie\" style=\"display:none\">'"
  , "        + '<select id=\"if\" class=\"bg-panel ba b--default fg-primary f6 tracked-wide\" style=\"padding:8px 12px\">'"
  , "          + '<option value=\"torznab\">format: torznab</option><option value=\"torrentday-json\">format: torrentday-json</option></select>'"
  , "        + '<input id=\"isp\" placeholder=\"search path /api\">'"
  , "        + '<input id=\"ic\" placeholder=\"cats 2000,5000\">'"
  , "        + '<button class=\"btn btn-primary\" onclick=\"addIndexer()\">Add indexer</button></div>');"
  , "  }).catch(function(){ section('indexers','<i>unavailable</i>'); });"
  , "}"
  , "function toggleAuthFields(){"
  , "  var cookie = el('ia').value==='cookie';"
  , "  el('ick').style.display = cookie ? '' : 'none';"
  , "  el('ik').style.display = cookie ? 'none' : '';"
  , "}"
  , "function addIndexer(){"
  , "  var cats=el('ic').value.split(',').map(function(s){ return parseInt(s,10); }).filter(function(n){ return !isNaN(n); });"
  , "  var body={Name:el('in').value,BaseUrl:el('ib').value,AuthType:el('ia').value,ResponseFormat:el('if').value,Categories:cats};"
  , "  if (el('ia').value==='cookie') { body.Cookie=el('ick').value; } else { body.ApiKey=el('ik').value; }"
  , "  if (el('isp').value) { body.SearchPath=el('isp').value; }"
  , "  api('/Indexers',{method:'POST',body:JSON.stringify(body)}).then(refresh).catch(showErr);"
  , "}"
  , "function testIndexer(b){ api('/Indexers/'+b.getAttribute('data-id')+'/Test',{method:'POST'}).then(function(d){ reqNotice(d.Ok?'TEST OK':'TEST FAILED'); refresh(); }).catch(showErr); }"
  , "function toggleIndexer(b){ var id=b.getAttribute('data-id'), en=b.getAttribute('data-en')==='1'; api('/Indexers/'+id+(en?'/Enable':'/Disable'),{method:'POST'}).then(refresh).catch(showErr); }"
  , "function deleteIndexer(b){ api('/Indexers/'+b.getAttribute('data-id'),{method:'DELETE'}).then(refresh).catch(showErr); }"
  , "function reqNotice(m){ el('reqnote').textContent = m; }"
  , "function searchAll(){"
  , "  var q=el('sq').value, ty=el('st').value;"
  , "  reqNotice('');"
  , "  api('/Indexers/Search?q='+encodeURIComponent(q)+'&type='+ty).then(function(d){"
  , "    var rs=d.Results||[], errs=d.Errors||{};"
  , "    LAST_RESULTS=rs;"
  , "    var errLine=Object.keys(errs).map(function(k){ return k+': '+errs[k]; }).join('; ');"
  , "    section('searchres', (errLine?'<div class=\"f7 fg-muted mb2\">'+esc(errLine)+'</div>':'')"
  , "      + (rs.length"
  , "        ? '<table><tr><th>Title</th><th>Size</th><th>Seeders</th><th>Indexer</th><th></th></tr>'"
  , "          + rs.map(function(r,ix){ return '<tr><td>'+esc(r.Title)+'</td><td>'+fmtBytes(r.SizeBytes)+'</td><td>'+(r.Seeders==null?'-':esc(r.Seeders))+'</td><td>'+esc(r.IndexerName)+'</td><td><button onclick=\"requestRelease('+ix+')\">Request</button></td></tr>'; }).join('')"
  , "          + '</table>'"
  , "        : '<i>no results</i>'));"
  , "  }).catch(function(){ section('searchres','<i>search failed</i>'); });"
  , "}"
  , "function requestRelease(ix){"
  , "  var r=LAST_RESULTS[ix]; if (!r) { return; }"
  , "  var body={Title:r.Title, Type:(el('st').value||'movie'), TrackerId:r.IndexerId, Release:r.Title};"
  , "  if (r.SizeBytes!=null) { body.SizeBytes=r.SizeBytes; }"
  , "  if (r.Magnet) { body.Magnet=r.Magnet; }"
  , "  if (r.DownloadUrl) { body.DownloadUrl=location.origin+r.DownloadUrl; }"
  , "  fetch('/Requests',{method:'POST',headers:{'X-Emby-Token':TOKEN,'Content-Type':'application/json'},body:JSON.stringify(body)})"
  , "    .then(function(resp){ return resp.json().catch(function(){ return {}; }).then(function(d){ return {code:resp.status, d:d}; }); })"
  , "    .then(function(o){"
  , "      if (o.code===201 || o.code===200) { reqNotice('REQUESTED ('+(o.d.Status||'')+')'); }"
  , "      else if (o.code===409) { reqNotice('ALREADY IN PROGRESS'); }"
  , "      else { reqNotice('REQUEST FAILED ('+o.code+')'); }"
  , "      if (currentTab==='requests') { renderRequests(); }"
  , "    })"
  , "    .catch(function(){ reqNotice('REQUEST FAILED'); });"
  , "}"
  , "function renderDlcfg(){"
  , "  if (!IS_ADMIN) { section('dlcfg','<i>admin only</i>'); el('dlstatus').textContent=''; return; }"
  , "  api('/Requests/Settings').then(function(s){"
  , "    section('dlcfg', dlFormHtml());"
  , "    if (!DL_LOADED) {"
  , "      el('deluge-url').value = s.DelugeUrl||'';"
  , "      el('deluge-password').value = s.DelugePassword||'';"
  , "      el('download-dir').value = s.DownloadDir||'';"
  , "      el('auto-approve-gb').value = (s.AutoApproveGb==null?'':s.AutoApproveGb);"
  , "      DL_LOADED = true;"
  , "    }"
  , "  }).catch(function(){ section('dlcfg','<i>unavailable</i>'); });"
  , "}"
  , "function dlFormHtml(){"
  , "  return '<div class=\"flex items-center flex-wrap gap-2\">'"
  , "    + '<input id=\"deluge-url\" placeholder=\"deluge url http://127.0.0.1:8112/json\">'"
  , "    + '<input id=\"deluge-password\" type=\"password\" placeholder=\"deluge password\">'"
  , "    + '<input id=\"download-dir\" placeholder=\"download dir\">'"
  , "    + '<input id=\"auto-approve-gb\" placeholder=\"auto-approve GB\">'"
  , "    + '<button class=\"btn btn-primary\" onclick=\"saveDl()\">Save</button>'"
  , "    + '<button class=\"btn btn-ghost\" onclick=\"testDl()\">Test</button>'"
  , "    + '</div>';"
  , "}"
  , "function dlBody(){"
  , "  var b={DelugeUrl:el('deluge-url').value, DelugePassword:el('deluge-password').value, DownloadDir:el('download-dir').value};"
  , "  var g=parseFloat(el('auto-approve-gb').value);"
  , "  if (!isNaN(g)) { b.AutoApproveGb=g; }"
  , "  return b;"
  , "}"
  , "function saveDl(){ api('/Requests/Settings',{method:'POST',body:JSON.stringify(dlBody())}).then(function(){ el('dlstatus').textContent='SAVED'; DL_LOADED=true; refresh(); }).catch(showErr); }"
  , "function testDl(){ api('/Requests/Settings/Test',{method:'POST',body:JSON.stringify(dlBody())}).then(function(d){ el('dlstatus').textContent = d.Ok ? 'TEST OK' : ('TEST FAILED: '+(d.Error||'')); }).catch(showErr); }"
  , "function renderMeta(){"
  , "  if (!IS_ADMIN) { section('metacfg','<i>admin only</i>'); el('metastatus').textContent=''; return; }"
  , "  api('/Metadata/Settings').then(function(s){"
  , "    section('metacfg', metaFormHtml());"
  , "    if (!META_LOADED) {"
  , "      el('tmdb-api-key').value = s.TmdbApiKey||'';"
  , "      el('fetch-images').value = (s.FetchImages ? 'true' : 'false');"
  , "      META_LOADED = true;"
  , "    }"
  , "  }).catch(function(){ section('metacfg','<i>unavailable</i>'); });"
  , "}"
  , "function metaFormHtml(){"
  , "  return '<div class=\"flex items-center flex-wrap gap-2\">'"
  , "    + '<input id=\"tmdb-api-key\" type=\"password\" placeholder=\"tmdb api key\">'"
  , "    + '<select id=\"fetch-images\" class=\"bg-panel ba b--default fg-primary f6 tracked-wide\" style=\"padding:8px 12px\">'"
  , "      + '<option value=\"true\">fetch images: yes</option><option value=\"false\">fetch images: no</option></select>'"
  , "    + '<button class=\"btn btn-primary\" onclick=\"saveMeta()\">Save</button>'"
  , "    + '<button class=\"btn btn-ghost\" onclick=\"testMeta()\">Test</button>'"
  , "    + '<button class=\"btn btn-ghost\" onclick=\"refreshMetaLib()\">Refresh library metadata</button>'"
  , "    + '</div>';"
  , "}"
  , "function metaBody(){"
  , "  return {TmdbApiKey: el('tmdb-api-key').value, FetchImages: el('fetch-images').value==='true'};"
  , "}"
  , "function saveMeta(){ api('/Metadata/Settings',{method:'POST',body:JSON.stringify(metaBody())}).then(function(){ el('metastatus').textContent='SAVED'; META_LOADED=true; refresh(); }).catch(showErr); }"
  , "function testMeta(){ api('/Metadata/Settings/Test',{method:'POST',body:JSON.stringify(metaBody())}).then(function(d){ el('metastatus').textContent = d.Ok ? ('TEST OK ('+d.Results+' results)') : ('TEST FAILED: '+(d.Error||'')); }).catch(showErr); }"
  , "function refreshMetaLib(){ api('/Library/Refresh?replaceAll=true',{method:'POST'}).then(function(){ el('metastatus').textContent='REFRESH STARTED'; }).catch(showErr); }"
  , "function rescan(){ api('/library/rescan', {method:'POST'}).then(refresh).catch(showErr); }"
  , "function genIntro(){ var n=el('intronote'); if(n){n.textContent='SCANNING...';} api('/items/Intro/Scan', {method:'POST'}).then(function(r){ if(n){n.textContent='scanned '+(r.Scanned||0)+', intros found '+(r.IntroFound||0)+', already cached '+(r.AlreadyCached||0);} }).catch(function(e){ if(n){n.textContent='FAILED: '+(e&&e.message?e.message:e);} showErr(e); }); }"
  , "function refreshMeta(){ api('/Library/Refresh?replaceAll=true', {method:'POST'}).then(refresh).catch(showErr); }"
  , "refresh();"
  , "setInterval(refresh, 10000);"
  , "</script></body></html>"
  ])
