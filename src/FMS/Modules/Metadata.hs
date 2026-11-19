module FMS.Modules.Metadata
  ( metadataModule
  , MonadMetadata (..)
  ) where

import FMS.Core.Module (Module (..), Segment (..), delete, get, post)
import FMS.Modules.Metadata.Monad (MonadMetadata (..), loadMetadataIntoState, loadMetadataSettingsIntoState)
import FMS.Modules.Metadata.Routes
  ( addItemTagsHandler
  , editMetadataHandler
  , genresHandler
  , getMetadataSettingsHandler
  , listItemTagsHandler
  , metadataHandler
  , personsHandler
  , postMetadataSettingsHandler
  , refreshMetadataHandler
  , removeItemTagHandler
  , seriesEpisodesHandler
  , seriesSearchHandler
  , studiosHandler
  , tagsHandler
  , testMetadataSettingsHandler
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
    , moduleInit = do
        loadMetadataIntoState
        loadMetadataSettingsIntoState   -- m35: seed metadata-settings.json from env on first boot
    , moduleRoutes =
        [ -- m35: settings routes are literals, before any capture (first-match-wins)
          get  [Lit "Metadata", Lit "Settings"] getMetadataSettingsHandler
        , post [Lit "Metadata", Lit "Settings"] postMetadataSettingsHandler
        , post [Lit "Metadata", Lit "Settings", Lit "Test"] testMetadataSettingsHandler
        -- m47: series view routes (literals before the items captures)
        , get [Lit "Series", Lit "Search"] seriesSearchHandler
        , get [Lit "Series", Capture "id", Lit "Episodes"] seriesEpisodesHandler
        , get  [Lit "Genres"] genresHandler
        , get  [Lit "Studios"] studiosHandler      -- milestone 4h
        , get  [Lit "Persons"] personsHandler      -- milestone 4h
        , get  [Lit "Tags"] tagsHandler            -- milestone 4h
        , get  [Lit "items", Capture "id", Lit "metadata"] metadataHandler
        , post [Lit "items", Capture "id", Lit "metadata"] editMetadataHandler          -- milestone 4k
        , post [Lit "items", Capture "id", Lit "metadata", Lit "refresh"] refreshMetadataHandler
        -- item tags API (literal "Tags" after the capture; captures keep
        -- their case, so DELETE /Items/1/Tags/Hd matches tag "Hd")
        , get    [Lit "items", Capture "id", Lit "Tags"] listItemTagsHandler
        , post   [Lit "items", Capture "id", Lit "Tags"] addItemTagsHandler
        , delete [Lit "items", Capture "id", Lit "Tags", Capture "tag"] removeItemTagHandler
        ]
    }
