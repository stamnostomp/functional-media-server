module FMS.Modules.Playlists
  ( playlistsModule
  , MonadPlaylists (..)
  ) where

import FMS.Core.Module (Module (..))
import FMS.Modules.Playlists.Monad (MonadPlaylists (..), loadPlaylistsIntoState)
import FMS.Modules.Playlists.Routes (playlistsRoutes)

-- | The playlists module (docs/roadmap.org milestone 10a, docs/todo.org
-- section 10): create/list/get/delete playlists and add/remove/list
-- their items, Jellyfin-shaped (@/Playlists...@). Init loads the
-- persisted map at @stateDir/playlists.json@ into a TVar under
-- "playlists.items" in the typed registry — the core never learns about
-- it (docs/modules.org) — and every mutation is persisted back (the
-- Stats/Metadata pattern).
playlistsModule :: Module
playlistsModule = Module
  { moduleName   = "playlists"
  , moduleInit   = loadPlaylistsIntoState
  , moduleRoutes = playlistsRoutes
  }
