module FMS.Modules.Admin.Types
  ( ActivityEntry(..)
  , ScheduledTask(..)
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.!=), (.=))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)

-- | One entry of the server activity log (docs/todo.org section 11,
-- milestone 11a): Jellyfin-shaped @/System/ActivityLog@ item. Entries are
-- kept in a ring buffer in memory (cap 'activityLogCap') and appended to
-- @stateDir/activity.log@ as JSON lines.
data ActivityEntry = ActivityEntry
  { aeId      :: !Int       -- ^ monotonically increasing id (ring buffer counter)
  , aeAt      :: !UTCTime   -- ^ when the event happened
  , aeType    :: !Text      -- ^ e.g. \"Server\", \"TaskRun\", \"LibraryScan\"
  , aeMessage :: !Text
  } deriving (Eq, Show)

instance ToJSON ActivityEntry where
  toJSON e = object
    [ "Id"      .= aeId e
    , "Time"    .= aeAt e
    , "Type"    .= aeType e
    , "Message" .= aeMessage e
    ]

instance FromJSON ActivityEntry where
  parseJSON = withObject "ActivityEntry" $ \o ->
    ActivityEntry
      <$> o .: "Id"
      <*> o .: "Time"
      <*> o .: "Type"
      <*> o .: "Message"

-- | A scheduled background task (docs/todo.org section 11, milestone
-- 11a): @/ScheduledTasks@ item. The scheduler in "FMS.Modules.Admin.Scheduler"
-- runs every task whose 'taskDue' predicate holds.
data ScheduledTask = ScheduledTask
  { stId              :: !Text            -- ^ stable id, e.g. \"library-rescan\"
  , stName            :: !Text            -- ^ human-readable name
  , stIntervalSeconds :: !Int             -- ^ run at most every this many seconds
  , stEnabled         :: !Bool
  , stLastRun         :: !(Maybe UTCTime) -- ^ Nothing = never run
  , stLastResult      :: !(Maybe Text)    -- ^ result message of the last run
  } deriving (Eq, Show)

instance ToJSON ScheduledTask where
  toJSON t = object
    [ "Id"              .= stId t
    , "Name"            .= stName t
    , "IntervalSeconds" .= stIntervalSeconds t
    , "Enabled"         .= stEnabled t
    , "LastRun"         .= stLastRun t
    , "LastResult"      .= stLastResult t
    ]

instance FromJSON ScheduledTask where
  parseJSON = withObject "ScheduledTask" $ \o ->
    ScheduledTask
      <$> o .:  "Id"
      <*> o .:  "Name"
      <*> o .:  "IntervalSeconds"
      <*> o .:? "Enabled" .!= True
      <*> o .:? "LastRun"
      <*> o .:? "LastResult"
