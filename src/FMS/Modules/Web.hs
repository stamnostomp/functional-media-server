module FMS.Modules.Web
  ( webModule
  , webPageHtml
  , dashboardPageHtml
  , dashboardBeforeCapture
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

import FMS.Core.Module (Module (..), Route (..), Segment (..), get)
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

-- | @GET /dashboard@ -- the built-in admin dashboard page
-- (milestone 19b), served regardless of 'configUiDir' (the Lit route
-- wins over the static capture, see 'dashboardBeforeCapture').
dashboardHandler :: [(Text, Text)] -> Request -> AppM Response
dashboardHandler _captures _req =
  pure (responseLBS status200
    [("Content-Type", "text/html; charset=utf-8")] dashboardPageHtml)

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
-- tasks / Plugins / Requests / Trackers sections plus Rescan and
-- Refresh-metadata buttons (m20c: Requests queue with Approve/Reject for
-- pending items, admin-only Trackers manager with add/enable/disable/
-- delete; the admin probe is GET /Users, 403 for non-admins).
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
  , "<main>"
  , "<div id='login' style='display:none'>"
  , "  <form onsubmit='return doLogin()' class='panel bg-panel b--default'>"
  , "    <input id='lu' placeholder='username' autocomplete='username'>"
  , "    <input id='lp' type='password' placeholder='password' autocomplete='current-password'>"
  , "    <button type='submit' class='btn btn-primary' style='margin-right:0'>Sign in</button>"
  , "  </form>"
  , "</div>"
  , "<section class='panel bg-panel b--default'>"
  , "  <h2>Actions</h2>"
  , "  <button class='btn btn-primary' onclick='rescan()'>Rescan library</button>"
  , "  <button class='btn btn-ghost' onclick='refreshMeta()'>Refresh metadata</button>"
  , "</section>"
  , "<section class='panel bg-panel b--default'><h2>Server</h2><div id='server'>...</div></section>"
  , "<section class='panel bg-panel b--default'><h2>Memory</h2><div id='mem'>...</div></section>"
  , "<section class='panel bg-panel b--default'><h2>Library</h2><div id='library'>...</div></section>"
  , "<section class='panel bg-panel b--default'><h2>Sessions</h2><div id='sessions'>...</div></section>"
  , "<section class='panel bg-panel b--default'><h2>Activity</h2><div id='activity'>...</div></section>"
  , "<section class='panel bg-panel b--default'><h2>Scheduled tasks</h2><div id='tasks'>...</div></section>"
  , "<section class='panel bg-panel b--default'><h2>Plugins</h2><div id='plugins'>...</div></section>"
  , "<section class='panel bg-panel b--default' id='requests-sec'><h2>Requests</h2><div id='requests'>...</div></section>"
  , "<section class='panel bg-panel b--default' id='trackers-sec'><h2>Trackers</h2><div id='trackers'>...</div></section>"
  , "</main>"
  , "<script>"
  , "var TOKEN = localStorage.getItem('fmsToken') || '';"
  , "var IS_ADMIN = false;"
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
  , "function section(id, html){ el(id).innerHTML = html; }"
  , "function showErr(e){ el('err').textContent = String(e && e.message ? e.message : e); }"
  , "function refresh(){"
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
  , "  fetch('/Users', {headers:{'X-Emby-Token':TOKEN}}).then(function(r){"
  , "    IS_ADMIN = (r.status===200);"
  , "    el('trackers-sec').style.display = IS_ADMIN ? '' : 'none';"
  , "    renderRequests();"
  , "    if (IS_ADMIN) { renderTrackers(); }"
  , "  }).catch(function(){ IS_ADMIN=false; renderRequests(); });"
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
  , "      ? '<table><tr><th>Title</th><th>User</th><th>Status</th><th>Size</th><th>Source</th><th>Created</th>'+(IS_ADMIN?'<th></th>':'')+'</tr>'"
  , "        + rs.map(function(r){"
  , "            var src=(r.SourceRelease||'')+(r.SourceIndexer?' ['+r.SourceIndexer+']':'');"
  , "            var btns=(IS_ADMIN && r.Status==='Pending') ? '<button data-id='+encodeURIComponent(r.Id)+' onclick=\"approveReq(this)\">Approve</button> <button data-id='+encodeURIComponent(r.Id)+' onclick=\"rejectReq(this)\">Reject</button>' : '';"
  , "            return '<tr><td>'+esc(r.Title)+(r.Year?' ('+esc(r.Year)+')':'')+'</td><td>'+esc(r.UserName||'')+'</td><td>'+esc(r.Status)+'</td><td>'+fmtBytes(r.SizeBytes)+'</td><td>'+esc(src||'-')+'</td><td>'+esc(String(r.CreatedAt||'').slice(0,19))+'</td>'+(IS_ADMIN?'<td>'+btns+'</td>':'')+'</tr>';"
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
  , "function rescan(){ api('/library/rescan', {method:'POST'}).then(refresh).catch(showErr); }"
  , "function refreshMeta(){ api('/Library/Refresh?replaceAll=true', {method:'POST'}).then(refresh).catch(showErr); }"
  , "refresh();"
  , "setInterval(refresh, 10000);"
  , "</script></body></html>"
  ])
