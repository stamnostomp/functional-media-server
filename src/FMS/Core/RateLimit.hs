module FMS.Core.RateLimit
  ( Bucket(..)
  , newBucket
  , refill
  , tryTake
  , rateLimitBucketsKey
  , requestCounterKey
  , requestIdHex
  , rateLimitClass
  , bucketKeyFor
  , retryAfterSeconds
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, diffUTCTime)
import Network.HTTP.Types (Method)
import Numeric (showHex)

-- | Registry key for the per-IP token buckets:
-- @TVar (HashMap Text Bucket)@ keyed by client IP (milestone 14a).
rateLimitBucketsKey :: Text
rateLimitBucketsKey = "ratelimit.buckets"

-- | Registry key for the global request counter TVar (Int) used to mint
-- request ids (milestone 14a).
requestCounterKey :: Text
requestCounterKey = "system.reqcounter"

-- | A token bucket (pure, milestone 14a). @rate@ is requests per MINUTE;
-- the bucket caps at @rate@ tokens (one minute's worth) and refills at
-- @rate \/ 60@ tokens per second.
data Bucket = Bucket
  { bucketTokens     :: !Double   -- ^ currently available tokens
  , bucketLastRefill :: !UTCTime  -- ^ last refill timestamp
  } deriving (Eq, Show)

-- | A fresh bucket at @now@, full (cap = rate).
newBucket :: Double -> UTCTime -> Bucket
newBucket rate now = Bucket rate now

-- | Tokens per second for a per-minute @rate@.
perSecond :: Double -> Double
perSecond rate = rate / 60

-- | Pure: refill the bucket up to @now@, capped at @rate@ tokens.
refill :: Double -> UTCTime -> Bucket -> Bucket
refill rate now b =
  let elapsed = realToFrac (diffUTCTime now (bucketLastRefill b)) :: Double
      gained  = elapsed * perSecond rate
  in Bucket (min rate (bucketTokens b + gained)) now

-- | Pure: try to take one token at @now@ (refilling first).
-- Returns @(allowed, bucket')@.
tryTake :: Double -> UTCTime -> Bucket -> (Bool, Bucket)
tryTake rate now b =
  let b' = refill rate now b
  in if bucketTokens b' >= 1
       then (True, b' { bucketTokens = bucketTokens b' - 1 })
       else (False, b')

-- | Render a request counter value as a 16-char zero-padded hex id
-- (same shape as the server id / session ids). Pure, exported for tests.
requestIdHex :: Int -> Text
requestIdHex n =
  let h = showHex n ""
  in T.pack (replicate (max 0 (16 - length h)) '0' ++ h)

-- | Milestone 14b: the rate-limit CLASS of a request. A @POST@ whose path
-- has @authenticatebyname@ (case-insensitive) among its first two segments
-- — i.e. both @POST /AuthenticateByName@ and the real route @POST
-- /Users/AuthenticateByName@ — gets the stricter @\"auth\"@ bucket;
-- everything else is @\"global\"@. Pure, exported for tests.
rateLimitClass :: Method -> [Text] -> Text
rateLimitClass m segs
  | m == "POST" && any (\s -> T.toLower s == "authenticatebyname") (take 2 segs) = "auth"
  | otherwise = "global"

-- | Milestone 14b: the bucket registry key for a (class, client IP) pair,
-- e.g. @auth:1.2.3.4@ vs @global:1.2.3.4@ — endpoint buckets are separate
-- from the global one. Pure, exported for tests.
bucketKeyFor :: Text -> Text -> Text
bucketKeyFor cls ip = cls <> ":" <> ip

-- | Milestone 14b: the @Retry-After@ header value (seconds) for a denied
-- request: the time until one token refills in a per-minute @rate@ bucket,
-- i.e. @ceil (60 / rate)@ clamped to >= 1. Pure, exported for tests.
retryAfterSeconds :: Double -> Int
retryAfterSeconds rate
  | rate <= 0 = 1
  | otherwise = max 1 (ceiling (60 / rate) :: Int)
