module FMS.Modules.Web
  ( webModule
  , webPageHtml
  ) where

import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBSC
import Data.Text (Text)
import Network.HTTP.Types (status200)
import Network.Wai (Request, Response, responseLBS)

import FMS.Core.Module (Module (..), Segment (..), get)
import FMS.Core.Monad (AppM)

-- | The built-in minimal web client (docs/todo.org section 13, milestone
-- 13/16): a single vanilla-JS page served at @/@ and @/web@. It lists the
-- library via @GET /items?search=...@, shows a thumbnail per item
-- (@/items/:id/thumb@), plays items in a @<video controls>@ tag via
-- @/stream/:id@, and links to the HLS playlist (@/videos/:id/main.m3u8@)
-- and the metadata endpoint (@/items/:id/metadata@). No external assets.
webModule :: Module
webModule = Module
  { moduleName   = "web"
  , moduleInit   = pure ()
  , moduleRoutes =
      [ get []            webHandler
      , get [Lit "web"]   webHandler
      ]
  }

-- | Both the root @/@ and @/web@ serve the same single-page client.
webHandler :: [(Text, Text)] -> Request -> AppM Response
webHandler _captures _req =
  pure (responseLBS status200
    [("Content-Type", "text/html; charset=utf-8")] webPageHtml)

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
