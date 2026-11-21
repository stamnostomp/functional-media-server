module FMS.Modules.Requests.Janitor
  ( startRequestsJanitor
  ) where

-- | m82: requests janitor thread. Every 60s it asks Deluge for the rich
-- torrent statuses (name/state/progress/rate/seeds/peers) and, for every
-- Downloading request whose live torrent matches but has ~0 progress AND 0
-- connected seeds, removes and re-adds the magnet (fresh peer state) at most
-- once per 10 minutes per request. Root cause fixed here: on this NAT,
-- outbound TCP to classic peer ports (6881 etc.) is blocked, so a torrent
-- whose first announce only yielded unreachable peers sits at 0% forever
-- with "Announce OK"; a fresh re-add + force_reannounce after the uTP/queue
-- config lands picks up reachable (high-port/uTP) seeds and downloads.

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException, catch)
import Control.Monad (forever, unless, void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.IORef (IORef, newIORef, modifyIORef', readIORef)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime, getCurrentTime)

import FMS.Core.Monad (AppEnv, AppM, logInfo, logWarn, runApp)
import FMS.Modules.AnnounceTrackers (currentAnnounceTrackers)
import FMS.Modules.Requests.Monad (MonadRequests (..), getRequestSettings)
import FMS.Modules.Requests.Push
  ( btihFromMagnet
  , delugeReaddTorrent
  , delugeTorrentStatusesRich
  )
import FMS.Modules.Requests.Types (Request (..), RequestSettings (..), RequestStatus (..), normalizeTitle)

-- | Stalled-torrent threshold: progress below 0.5% with 0 connected seeds.
stalledProgress :: Double
stalledProgress = 0.5

-- | Minimum seconds between re-adds of the same request.
readdCooldownSecs :: NominalDiffTime
readdCooldownSecs = 600

-- | Fork the janitor loop (called once from the requests module init).
startRequestsJanitor :: AppM ()
startRequestsJanitor = do
  env <- ask
  liftIO $ do
    ref <- newIORef (HM.empty :: HashMap Text UTCTime)
    _ <- forkIO (janitorLoop env ref `catch` \e ->
          void (runApp env (logWarn ("requests janitor: thread died: " <> T.pack (show (e :: SomeException))))))
    pure ()

janitorLoop :: AppEnv -> IORef (HashMap Text UTCTime) -> IO ()
janitorLoop env ref = forever $ do
  threadDelay 60000000   -- 60s
  void (runApp env (janitorPass ref)) `catch` \e ->
    void (runApp env (logWarn ("requests janitor: pass failed: " <> T.pack (show (e :: SomeException)))))

janitorPass :: IORef (HashMap Text UTCTime) -> AppM ()
janitorPass ref = do
  s <- getRequestSettings
  unless (T.null (rsDelugeUrl s)) $ do
    reqs <- listRequests
    eres <- liftIO (delugeTorrentStatusesRich (rsDelugeUrl s) (rsDelugePassword s))
    case eres of
      Left _ -> pure ()
      Right sts -> do
        trackers <- currentAnnounceTrackers
        now <- liftIO getCurrentTime
        let dir = T.unpack (rsDownloadDir s)
            downloading = [ r | r <- reqs, reqStatus r == Downloading, isJust (reqMagnet r) ]
        mapM_ (recoverOne s dir trackers sts ref now) downloading

-- | One stalled-download recovery attempt for a request.
recoverOne
  :: RequestSettings
  -> FilePath
  -> [Text]
  -> [(Text, Text, Double, Double, Int, Int)]
  -> IORef (HashMap Text UTCTime)
  -> UTCTime
  -> Request
  -> AppM ()
recoverOne s dir trackers sts ref now r =
  case matchRich (reqTitle r) (reqSourceRelease r) sts of
    Just (_name, _state, prog, _rate, seeds, _peers)
      | prog < stalledProgress && seeds == 0 ->
          tryReadd s dir trackers ref now r
    _ -> pure ()

-- | Re-add at most once per cooldown window per request.
tryReadd :: RequestSettings -> FilePath -> [Text] -> IORef (HashMap Text UTCTime) -> UTCTime -> Request -> AppM ()
tryReadd s dir trackers ref now r = do
  last <- liftIO (readIORef ref)
  when (maybe True (\t -> diffUTCTime now t >= readdCooldownSecs) (HM.lookup (reqId r) last)) $
    case (reqMagnet r, reqMagnet r >>= btihFromMagnet) of
      (Just mag, Just hash) -> do
        eres <- liftIO (delugeReaddTorrent (rsDelugeUrl s) (rsDelugePassword s) dir mag hash trackers)
        case eres of
          Right () -> do
            liftIO (modifyIORef' ref (HM.insert (reqId r) now))
            logInfo ("requests janitor: re-added stalled torrent for request " <> reqId r <> " (" <> reqTitle r <> ")")
          Left err ->
            logWarn ("requests janitor: re-add failed for " <> reqTitle r <> ": " <> T.pack err)
      _ -> pure ()

-- | m82: match a request against the rich status list (same normalized
-- equality/containment rule as matchTorrentProgress, on 6-tuples).
matchRich
  :: Text
  -> Maybe Text
  -> [(Text, Text, Double, Double, Int, Int)]
  -> Maybe (Text, Text, Double, Double, Int, Int)
matchRich title mRelease torrents =
  case filter matchesOne torrents of
    (t : _) -> Just t
    []      -> Nothing
  where
    nt = normalizeTitle title
    cands = nt : maybe [] (\rel -> [normalizeTitle rel]) mRelease
    matchesOne (name, _st, _prog, _rate, _seeds, _peers) =
      let nn = normalizeTitle name
      in any (\c -> not (T.null c) && (nn == c || T.isInfixOf c nn || T.isInfixOf nn c)) cands
