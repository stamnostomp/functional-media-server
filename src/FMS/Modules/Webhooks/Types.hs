module FMS.Modules.Webhooks.Types
  ( Webhook(..)
  , eventMatches
  ) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.!=), (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime (..))
import Data.Time.Calendar (fromGregorian)

-- | One registered webhook (milestone 12, docs/todo.org section 12),
-- persisted in @stateDir/webhooks.json@. @whEvents@ is the list of event
-- names this hook subscribes to; empty or containing @\"*\"@ means all
-- events (see 'eventMatches').
data Webhook = Webhook
  { whId       :: !Text            -- ^ minted server-side (16 hex)
  , whUrl      :: !Text            -- ^ target URL, POSTed to on events
  , whEvents   :: ![Text]          -- ^ subscribed event names ("*" = all)
  , whEnabled  :: !Bool
  , whSecret   :: !(Maybe Text)    -- ^ sent as X-Webhook-Secret header when set
  , whAddedAt  :: !UTCTime
  } deriving (Eq, Show)

instance ToJSON Webhook where
  toJSON w = object
    [ "Id"       .= whId w
    , "Url"      .= whUrl w
    , "Events"   .= whEvents w
    , "Enabled"  .= whEnabled w
    , "Secret"   .= whSecret w
    , "AddedAt"  .= whAddedAt w
    ]

instance FromJSON Webhook where
  parseJSON = withObject "Webhook" $ \o ->
    Webhook
      <$> o .:  "Id"
      <*> o .:  "Url"
      <*> o .:? "Events"  .!= []
      <*> o .:? "Enabled" .!= True
      <*> o .:? "Secret"  .!= Nothing
      <*> o .:? "AddedAt" .!= UTCTime (fromGregorian 1970 1 1) 0

-- | Pure (milestone 12): does this webhook subscribe to @event@?
-- An empty event list or one containing @\"*\"@ matches everything;
-- otherwise case-insensitive membership.
eventMatches :: [Text] -> Text -> Bool
eventMatches [] _ = True
eventMatches evts e =
  "*" `elem` norm evts || T.toCaseFold e `elem` norm evts
  where norm = map T.toCaseFold
