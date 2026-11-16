module FMS.Modules.Segments
  ( segmentsModule
  , MonadSegments (..)
  ) where

import FMS.Core.Module (Module (..))
import FMS.Modules.Segments.Monad (MonadSegments (..), loadSegmentsIntoState)
import FMS.Modules.Segments.Routes (segmentsRoutes)

-- | The media-segments module (docs/todo.org section 6, milestone 6h):
-- intro/outro/preview/... skip markers stored manually per item
-- (auto-detection is out of scope — documented), Jellyfin-shaped
-- (@/items/:id/MediaSegments...@), persisted at
-- @stateDir/mediaSegments.json@ as @{itemId: [MediaSegment]}@ in a TVar
-- under \"segments.map\" in the typed registry; every mutation is
-- persisted back (the Collections pattern).
segmentsModule :: Module
segmentsModule = Module
  { moduleName   = "segments"
  , moduleInit   = loadSegmentsIntoState
  , moduleRoutes = segmentsRoutes
  }
