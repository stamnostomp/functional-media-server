{-# LANGUAGE OverloadedStrings #-}

-- | Parental-control policy types and pure helpers (milestone 9).
--
-- A 'UserPolicy' carries an optional maximum parental rating, a list of
-- blocked tags and access schedules (same-day windows only, documented on
-- 'scheduleAllowsNow').  All helpers here are pure and exported for tests.
module FMS.Core.Types.Policy
  ( UserPolicy(..)
  , AccessSchedule(..)
  , emptyUserPolicy
  , ratingRank
  , ratingAllowed
  , tagsAllowed
  , scheduleAllowsNow
  , itemAllowedByPolicy
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), object, withObject, (.=), (.:?), (.!=))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (dayOfWeek)
import Data.Time.Clock (UTCTime, utctDay, utctDayTime)
import Data.Time.LocalTime (timeToTimeOfDay, todHour)

import FMS.Modules.Metadata.Types (MediaMetadata(..))

-- | Same-day access window: day 0=Sunday..6=Saturday, hours 0-23,
-- window is [start, end) on that day.  Overnight windows are NOT supported.
data AccessSchedule = AccessSchedule
  { schedDayOfWeek :: Int
  , schedStartHour :: Int
  , schedEndHour   :: Int
  } deriving (Eq, Show)

data UserPolicy = UserPolicy
  { policyMaxRating   :: Maybe Text       -- ^ maximum allowed official rating (e.g. "PG-13")
  , policyBlockedTags :: [Text]           -- ^ tags the user may not see
  , policySchedules   :: [AccessSchedule] -- ^ access windows; empty = always allowed
  } deriving (Eq, Show)

emptyUserPolicy :: UserPolicy
emptyUserPolicy = UserPolicy Nothing [] []

instance ToJSON AccessSchedule where
  toJSON (AccessSchedule d s e) =
    object [ "DayOfWeek" .= d, "StartHour" .= s, "EndHour" .= e ]

instance FromJSON AccessSchedule where
  parseJSON = withObject "AccessSchedule" $ \v -> AccessSchedule
    <$> (v .:? "DayOfWeek" .!= 0)
    <*> (v .:? "StartHour" .!= 0)
    <*> (v .:? "EndHour" .!= 24)

instance ToJSON UserPolicy where
  toJSON (UserPolicy mr bt sc) =
    object [ "MaxParentalRating" .= mr
           , "BlockedTags" .= bt
           , "AccessSchedules" .= sc ]

instance FromJSON UserPolicy where
  parseJSON = withObject "UserPolicy" $ \v -> UserPolicy
    <$> (v .:? "MaxParentalRating")
    <*> (v .:? "BlockedTags" .!= [])
    <*> (v .:? "AccessSchedules" .!= [])

-- | Rank of a parental rating; higher = more restrictive.  Unknown ratings
-- map to 99 (most restrictive).  Comparison is case-insensitive and ignores
-- surrounding whitespace.
ratingRank :: Text -> Int
ratingRank t = case norm of
  "G"      -> 1
  "PG"     -> 2
  "PG-13"  -> 3
  "R"      -> 4
  "NC-17"  -> 5
  "TV-Y"   -> 1
  "TV-Y7"  -> 2
  "TV-G"   -> 2
  "TV-PG"  -> 3
  "TV-14"  -> 4
  "TV-MA"  -> 5
  _        -> 99
  where norm = T.toUpper (T.strip t)

-- | Is an item rating allowed under a maximum rating?
-- Nothing maximum allows everything; an unrated item (Nothing) is allowed
-- (documented choice: unrated content passes, Jellyfin-style).
ratingAllowed :: Maybe Text -> Maybe Text -> Bool
ratingAllowed Nothing    _       = True
ratingAllowed (Just _)   Nothing = True
ratingAllowed (Just mx)  (Just r) = ratingRank r <= ratingRank mx

-- | True when none of the item tags are blocked (case-insensitive).
tagsAllowed :: [Text] -> [Text] -> Bool
tagsAllowed blocked itemTags =
  not (any (\b -> any (\t -> T.toLower b == T.toLower t) itemTags) blocked)

-- | Does 'now' fall inside at least one schedule window?  Empty schedule
-- list means always allowed.  Windows are same-day: [start, end) on the
-- given weekday (0=Sunday..6=Saturday, matching 'fromEnum' of 'WeekDay');
-- overnight windows are not supported.
scheduleAllowsNow :: [AccessSchedule] -> UTCTime -> Bool
scheduleAllowsNow [] _ = True
scheduleAllowsNow scheds now =
  let -- Data.Time's WeekDay enum is Monday=1..Sunday=7; mod 7 gives Sunday=0..Saturday=6
      dow  = fromEnum (dayOfWeek (utctDay now)) `mod` 7
      hour = todHour (timeToTimeOfDay (utctDayTime now))
      inWindow (AccessSchedule d s e) = d == dow && s <= hour && hour < e
  in any inWindow scheds

-- | Full policy check for one item against its metadata (if any).
-- Tags and official rating are checked; schedules are handled separately
-- via 'scheduleAllowsNow' because they gate all access, not one item.
itemAllowedByPolicy :: UserPolicy -> Maybe MediaMetadata -> Bool
itemAllowedByPolicy pol mMeta =
  tagsAllowed (policyBlockedTags pol) (maybe [] metaTags mMeta)
  && ratingAllowed (policyMaxRating pol) (mMeta >>= metaOfficialRating)
