{-# LANGUAGE OverloadedStrings #-}

-- | Server log entries and the pure log helpers (milestone 11b,
-- docs/todo.org section 12): a ring buffer of recent log lines kept in
-- the typed registry under \"system.logbuf\" and served by
-- @GET /System/Logs/Log@, plus the level filter applied before printing.
module FMS.Core.Types.Log
  ( LogEntry(..)
  , logBufferCap
  , pushLogEntry
  , logLevelAllows
  , logRank
  ) where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)

-- | One server log line (milestone 11b).
data LogEntry = LogEntry
  { leTime    :: !UTCTime
  , leLevel   :: !Text   -- ^ "debug" | "info" | "warn" | "error"
  , leMessage :: !Text
  } deriving (Eq, Show)

instance ToJSON LogEntry where
  toJSON e = object
    [ "Timestamp" .= leTime e
    , "Level"     .= leLevel e
    , "Message"   .= leMessage e
    ]

-- | The ring-buffer capacity for "system.logbuf".
logBufferCap :: Int
logBufferCap = 500

-- | Pure ring-buffer push (mirrors Admin's 'pushActivity'): append the
-- entry at the END (newest last) and drop the oldest entries from the
-- front when over @cap@. Callers reading newest-first reverse the list.
pushLogEntry :: Int -> [LogEntry] -> LogEntry -> [LogEntry]
pushLogEntry cap buf e =
  let buf' = buf ++ [e]
  in drop (max 0 (length buf' - cap)) buf'

-- | Numeric rank of a level: debug=0 < info=1 < warn=2 < error=3.
-- Unknown entry levels are treated as info (1).
logRank :: Text -> Int
logRank t = case T.toCaseFold (T.strip t) of
  "debug"   -> 0
  "info"    -> 1
  "warn"    -> 2
  "warning" -> 2
  "error"   -> 3
  _         -> 1

-- | Does the configured level allow this entry? Order debug < info <
-- warn < error; an entry whose level is lower than the configured level
-- is suppressed. An UNKNOWN configured level allows everything.
logLevelAllows :: Text -> Text -> Bool
logLevelAllows cfgLevel entryLevel =
  let c = T.toCaseFold (T.strip cfgLevel)
  in if c `notElem` ["debug", "info", "warn", "warning", "error"]
       then True
       else logRank entryLevel >= logRank cfgLevel
