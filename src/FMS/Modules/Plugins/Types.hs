module FMS.Modules.Plugins.Types
  ( Plugin (..)
  , builtinPlugins
  , overlayPlugins
  , registerHook
  , unregisterHook
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:?), (.!=), (.=))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (nub)
import Data.Text (Text)

-- | One plugin registry entry (plugin system skeleton, Jellyfin =/Plugins=
-- parity). This is a REGISTRY, not dynamic loading: built-in entries with
-- persisted enable/disable state.
data Plugin = Plugin
  { plugId          :: !Text   -- ^ stable id, e.g. "webhooks"
  , plugName        :: !Text   -- ^ display name, e.g. "Webhooks"
  , plugVersion     :: !Text
  , plugDescription :: !Text
  , plugEnabled     :: !Bool
  } deriving (Eq, Show)

-- | API/persistence JSON, PascalCase keys (the API convention).
instance ToJSON Plugin where
  toJSON p = object
    [ "Id"          .= plugId p
    , "Name"        .= plugName p
    , "Version"     .= plugVersion p
    , "Description" .= plugDescription p
    , "Enabled"     .= plugEnabled p
    ]

-- | Missing keys default so partial plugins.json entries still load.
instance FromJSON Plugin where
  parseJSON = withObject "Plugin" $ \o ->
    Plugin
      <$> o .:? "Id"          .!= ""
      <*> o .:? "Name"        .!= ""
      <*> o .:? "Version"     .!= ""
      <*> o .:? "Description" .!= ""
      <*> o .:? "Enabled"     .!= True

-- | The built-in plugin registrations, seeded at first boot. "webhooks"
-- is implemented (milestone 12); "livetv" and "dlna" are placeholders
-- (not implemented, disabled).
builtinPlugins :: [Plugin]
builtinPlugins =
  [ Plugin { plugId = "webhooks", plugName = "Webhooks", plugVersion = "1.0.0"
           , plugDescription = "Outgoing webhook notifications (implemented)."
           , plugEnabled = True }
  , Plugin { plugId = "livetv", plugName = "Live TV", plugVersion = "0.0.0"
           , plugDescription = "Live TV tuners and guide (not implemented)."
           , plugEnabled = False }
  , Plugin { plugId = "dlna", plugName = "DLNA", plugVersion = "0.0.0"
           , plugDescription = "DLNA/UPnP media sharing (not implemented)."
           , plugEnabled = False }
  ]

-- | Pure: overlay the persisted plugin map onto the built-ins. Persisted
-- entries win for known ids (so an enabled/disabled state survives a
-- restart); built-ins fill in anything missing; UNKNOWN persisted ids
-- are kept too, so future real plugins survive restarts.
overlayPlugins :: [Plugin] -> HashMap Text Plugin -> HashMap Text Plugin
overlayPlugins builtins persisted =
  HM.union persisted (HM.fromList [ (plugId b, b) | b <- builtins ])

-- | Hook registry skeleton: an in-process map of hook name -> registered
-- plugin ids, kept in the typed state registry under "plugins.hooks".
-- Pure helpers (exported for tests); no module calls them yet.

-- | Pure: register @pid@ for @hook@ (deduped, order preserved).
registerHook :: Text -> Text -> HashMap Text [Text] -> HashMap Text [Text]
registerHook hook pid = HM.insertWith (\new old -> nub (old ++ new)) hook [pid]

-- | Pure: remove @pid@ from @hook@ (the hook key stays, possibly empty).
unregisterHook :: Text -> Text -> HashMap Text [Text] -> HashMap Text [Text]
unregisterHook hook pid = HM.adjust (filter (/= pid)) hook
