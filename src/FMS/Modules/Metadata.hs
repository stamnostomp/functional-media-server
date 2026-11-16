module FMS.Modules.Metadata
  ( metadataModule
  , MonadMetadata (..)
  ) where

import FMS.Core.Module (Module (..), Segment (..), get, post)
import FMS.Modules.Metadata.Monad (MonadMetadata (..), loadMetadataIntoState)
import FMS.Modules.Metadata.Routes
  ( editMetadataHandler
  , genresHandler
  , metadataHandler
  , personsHandler
  , refreshMetadataHandler
  , studiosHandler
  , tagsHandler
  )

-- | The metadata module (docs/roadmap.org milestone 4, docs/todo.org
-- section 3): NFO sidecar parsing plus a disk-backed metadata cache, with
-- a TMDB remote fallback (milestone 4c) when no NFO exists.
-- Init loads the persisted map at @stateDir/metadata.json@ into a TVar
-- under "metadata.cache" in the typed registry — the core never learns
-- about it (docs/modules.org). Remote providers plug into
-- 'refreshMetadata' (TMDB done, TvMaze later).
metadataModule :: Module
metadataModule =
  Module
    { moduleName = "metadata"
    , moduleInit = loadMetadataIntoState
    , moduleRoutes =
        [ get  [Lit "Genres"] genresHandler
        , get  [Lit "Studios"] studiosHandler      -- milestone 4h
        , get  [Lit "Persons"] personsHandler      -- milestone 4h
        , get  [Lit "Tags"] tagsHandler            -- milestone 4h
        , get  [Lit "items", Capture "id", Lit "metadata"] metadataHandler
        , post [Lit "items", Capture "id", Lit "metadata"] editMetadataHandler          -- milestone 4k
        , post [Lit "items", Capture "id", Lit "metadata", Lit "refresh"] refreshMetadataHandler
        ]
    }
