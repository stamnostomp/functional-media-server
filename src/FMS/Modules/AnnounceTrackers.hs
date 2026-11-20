module FMS.Modules.AnnounceTrackers
  ( announceTrackersModule
  , parseTrackerList
  , currentAnnounceTrackers
  , refreshAnnounceTrackersNow
  , startAnnounceTrackerRefresher
  , announceTrackersKey
  , announceTrackersFilePath
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Control.Exception (SomeException, try)
import Control.Monad (forever, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, asks)
import Data.Aeson (Value, object, (.=))
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Network.Wai (Request, Response)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

import FMS.Core.Module (Module (..), Segment (..), get, jsonResponse, post)
import FMS.Core.Monad (AppEnv (..), AppError, AppM, MonadLog (..), envConfig, getState, putState, runApp)
import FMS.Core.Types.Config (Config (..))
import FMS.Modules.Auth.Monad (requireAdmin)
import FMS.Modules.Trackers.Torznab (fetchUrl)

-- | m62: public announce-tracker list (default: ngosang/trackerslist
-- trackers_best.txt, refreshed every 24 h). These are BitTorrent
-- announce URLs attached to Deluge adds (@add_trackers@ option), NOT
-- searchable indexers -- the /Trackers module stays Jackett-style.
-- Persisted in @stateDir/announce-trackers.json@ and kept in a TVar
-- under 'announceTrackersKey'.
announceTrackersModule :: Module
announceTrackersModule = Module
  { moduleName   = "announceTrackers"
  , moduleInit   = loadAnnounceTrackersIntoState
  , moduleRoutes =
      [ get  [Lit "AnnounceTrackers"] listAnnounceTrackersHandler
      , post [Lit "AnnounceTrackers", Lit "Refresh"] refreshAnnounceTrackersHandler
      ]
  }

announceTrackersKey :: Text
announceTrackersKey = "announceTrackers"

announceTrackersFilePath :: Config -> FilePath
announceTrackersFilePath cfg = configStateDir cfg </> "announce-trackers.json"

-- | The in-memory shape: when the list was last fetched, and the URLs.
type AnnounceTrackers = (Maybe UTCTime, [Text])

announceTrackersToJson :: AnnounceTrackers -> Value
announceTrackersToJson (mt, urls) = object [ "fetchedAt" .= mt, "urls" .= urls ]

announceTrackersParser :: Value -> Parser AnnounceTrackers
announceTrackersParser = Aeson.withObject "AnnounceTrackers" $ \o ->
  (,) <$> (o Aeson..:? "fetchedAt") <*> (o Aeson..: "urls")

-- | Pure: parse a trackerslist plain-text body into announce URLs.
-- Blank lines and comments (#) are dropped, entries are deduped and
-- capped at 100; only lines with a known scheme are kept.
parseTrackerList :: Text -> [Text]
parseTrackerList body =
  take 100 (nub [ u | l <- T.lines body
                   , let u = T.strip l
                   , not (T.null u)
                   , not ("#" `T.isPrefixOf` u)
                   , any (`T.isPrefixOf` u) ["udp://", "http://", "https://", "ws://", "wss://"] ])

-- | Load the persisted list (if any) into the state TVar.
loadAnnounceTrackersIntoState :: AppM ()
loadAnnounceTrackersIntoState = do
  cfg <- asks envConfig
  let path = announceTrackersFilePath cfg
  exists <- liftIO (doesFileExist path)
  stored <- if not exists
    then pure (Nothing, [] :: [Text])
    else do
      raw <- liftIO (LBS.readFile path)
      pure (case Aeson.decode raw >>= parseMaybe announceTrackersParser of
        Just st -> st
        Nothing -> (Nothing, []))
  tv <- liftIO (newTVarIO stored)
  putState announceTrackersKey tv

-- | The current announce tracker URLs (empty list when never fetched).
currentAnnounceTrackers :: AppM [Text]
currentAnnounceTrackers = do
  mTv <- getState announceTrackersKey :: AppM (Maybe (TVar AnnounceTrackers))
  case mTv of
    Nothing -> pure []
    Just tv -> do
      (_, urls) <- liftIO (readTVarIO tv)
      pure urls

persistAnnounceTrackers :: Config -> AnnounceTrackers -> IO ()
persistAnnounceTrackers cfg st =
  LBS.writeFile (announceTrackersFilePath cfg) (Aeson.encode (announceTrackersToJson st))

-- | Fetch the configured list URL once and store it. Returns True on
-- success. Never throws.
refreshAnnounceTrackersNow :: AppM Bool
refreshAnnounceTrackersNow = do
  cfg <- asks envConfig
  let url = T.unpack (configAnnounceListUrl cfg)
  eres <- liftIO (fetchUrl [] 15000000 url)
  case eres of
    Left err -> do
      logWarn ("announce-trackers: fetch failed: " <> T.pack err)
      pure False
    Right (code, body)
      | code /= 200 -> do
          logWarn ("announce-trackers: HTTP " <> T.pack (show code) <> " from " <> T.pack url)
          pure False
      | otherwise -> do
          let urls = parseTrackerList (TE.decodeUtf8With TEE.lenientDecode (LBS.toStrict body))
          now0 <- liftIO getCurrentTime
          let st' = (Just now0, urls)
          mTv <- getState announceTrackersKey :: AppM (Maybe (TVar AnnounceTrackers))
          case mTv of
            Nothing -> pure False
            Just tv -> do
              liftIO (atomically (writeTVar tv st'))
              liftIO (persistAnnounceTrackers cfg st')
              logInfo ("announce-trackers: refreshed " <> T.pack (show (length urls)) <> " trackers from " <> T.pack url)
              pure True

-- | Fork the refresher daemon (called once from runServer): refresh
-- immediately when the stored list is stale (never fetched or older
-- than the configured interval), then re-check every interval.
-- 0 disables the daemon. The loop body is wrapped in 'try'.
startAnnounceTrackerRefresher :: AppM ()
startAnnounceTrackerRefresher = do
  cfg <- asks envConfig
  env <- ask
  let secs = configAnnounceListRefreshSecs cfg
  when (secs > 0) $ do
    _ <- liftIO (forkIO (refresherLoop env secs))
    pure ()

refresherLoop :: AppEnv -> Int -> IO ()
refresherLoop env secs = forever $ do
  _ <- try (runApp env refreshIfStale) :: IO (Either SomeException (Either AppError ()))
  threadDelay (secs * 1000000)

refreshIfStale :: AppM ()
refreshIfStale = do
  cfg <- asks envConfig
  mTv <- getState announceTrackersKey :: AppM (Maybe (TVar AnnounceTrackers))
  case mTv of
    Nothing -> pure ()
    Just tv -> do
      (mt, _) <- liftIO (readTVarIO tv)
      now <- liftIO getCurrentTime
      let stale = case mt of
            Nothing -> True
            Just t  -> diffUTCTime now t > fromIntegral (configAnnounceListRefreshSecs cfg)
      when stale $ do
        _ <- refreshAnnounceTrackersNow
        pure ()

-- routes --------------------------------------------------------------------

listAnnounceTrackersHandler :: [(Text, Text)] -> Request -> AppM Response
listAnnounceTrackersHandler _captures _req = do
  mTv <- getState announceTrackersKey :: AppM (Maybe (TVar AnnounceTrackers))
  st <- case mTv of
    Nothing -> pure (Nothing, [])
    Just tv -> liftIO (readTVarIO tv)
  pure (jsonResponse (object
    [ "FetchedAt" .= fst st
    , "Count" .= length (snd st)
    , "Urls" .= snd st
    ]))

refreshAnnounceTrackersHandler :: [(Text, Text)] -> Request -> AppM Response
refreshAnnounceTrackersHandler _captures req = do
  _ <- requireAdmin req
  ok <- refreshAnnounceTrackersNow
  mTv <- getState announceTrackersKey :: AppM (Maybe (TVar AnnounceTrackers))
  st <- case mTv of
    Nothing -> pure (Nothing, [])
    Just tv -> liftIO (readTVarIO tv)
  pure (jsonResponse (object
    [ "Ok" .= ok
    , "FetchedAt" .= fst st
    , "Count" .= length (snd st)
    ]))
