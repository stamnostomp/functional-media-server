module FMS.Modules.Playback
  ( playbackModule
  , playbackRoutes
  ) where

import FMS.Core.Module (Module(..))
import FMS.Modules.Playback.Routes (playbackRoutes)

-- | The playback module (docs/roadmap.org milestone 6, docs/todo.org
-- section 6): @POST /items/:id/PlaybackInfo@ answers the Jellyfin
-- playback-decision query backed by ffprobe stream inspection, and
-- @GET /items/:id/MediaStreams@ exposes the probed streams. It only
-- needs the 'MonadLibrary' capability — the core never learns about it
-- (docs/modules.org). ffprobe is found on PATH at runtime; the Nix
-- package wraps the @fms@ binary with ffprobe on its PATH (flake.nix).
--
-- Note: module init deliberately does NOT touch the process-global
-- locale encoding anymore. The milestone-6a bug was a 500 on
-- unprobeable items because a latin1 locale + a non-latin1 char in a log
-- string made hPutChar throw; subprocess output is now read as raw bytes
-- (see "FMS.Core.Process") and log strings are ASCII-only.
playbackModule :: Module
playbackModule = Module
  { moduleName   = "playback"
  , moduleInit   = pure ()
  , moduleRoutes = playbackRoutes
  }
