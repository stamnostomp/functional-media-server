module FMS.Modules.Stats
  ( statsModule
  , MonadStats (..)
  ) where

import FMS.Core.Module (Module (..), Segment (..), get, post)
import FMS.Modules.Stats.Monad (MonadStats (..), loadStatsIntoState)
import FMS.Modules.Stats.Routes
  ( continueWatchingHandler
  , newHandler
  , popularHandler
  , progressHandler
  )

-- | The stats module (docs/roadmap.org milestone 5): play counts and
-- playback progress, persisted as JSON at @stateDir/stats.json@. Init
-- loads the persisted map into a TVar under "stats.items" in the typed
-- registry — the core never learns about it (docs/modules.org).
statsModule :: Module
statsModule =
  Module
    { moduleName = "stats"
    , moduleInit = loadStatsIntoState
    , moduleRoutes =
        [ post [Lit "progress"] (\_ req -> progressHandler req)
        , get [Lit "continuewatching"] (\_ _ -> continueWatchingHandler)
        , get [Lit "popular"] (\_ _ -> popularHandler)
        , get [Lit "new"] (\_ _ -> newHandler)
        ]
    }
