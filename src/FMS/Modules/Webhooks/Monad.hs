module FMS.Modules.Webhooks.Monad
  ( MonadWebhooks (..)
  , webhooksStateKey
  , webhooksFilePath
  , loadWebhooksIntoState
  , newWebhookId
  , fireWebhooks
  , deliverWebhook
  , webhookBody
  , webhookTimeoutMicros
  ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar, readTVarIO)
import Control.Exception (SomeException, try)
import Control.Monad (unless, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, asks)
import Data.Aeson (Value, encode, object, (.=))
import qualified Data.ByteString.Lazy as LBS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime, getCurrentTime)
import Network.HTTP.Client
  ( Request, RequestBody (..), Response, managerResponseTimeout
  , method, parseRequest, requestBody, requestHeaders, responseStatus
  , responseTimeoutMicro
  )
import Network.HTTP.Conduit (httpLbs, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types (statusCode)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.Random (newStdGen, randomRs)

import FMS.Core.Monad (AppM, MonadLog (..), envConfig, getState, putState, runApp)
import FMS.Core.Types.Config (Config (..))
import FMS.Modules.Webhooks.Persistence (loadWebhooks, saveWebhooks)
import FMS.Modules.Webhooks.Types (Webhook (..), eventMatches)

-- | Namespaced key for the @TVar (HashMap Text Webhook)@ in the typed
-- registry (docs/modules.org: the core never knows module state).
webhooksStateKey :: Text
webhooksStateKey = "webhooks.map"

-- | Where the webhook map is persisted: @stateDir/webhooks.json@.
webhooksFilePath :: Config -> FilePath
webhooksFilePath cfg = configStateDir cfg </> "webhooks.json"

-- | Response-header timeout for webhook POSTs: 5s in microseconds.
webhookTimeoutMicros :: Int
webhookTimeoutMicros = 5000000

-- | Capability for the webhook registry (milestone 12,
-- docs/todo.org section 12). The 'Bool' of 'deleteWebhook' means
-- "the webhook existed" — routes map 'False' to 404.
class Monad m => MonadWebhooks m where
  listWebhooks   :: m [Webhook]                   -- ^ all webhooks, oldest first
  getWebhook     :: Text -> m (Maybe Webhook)     -- ^ by id
  createWebhook  :: Text -> [Text] -> Bool -> Maybe Text -> m Webhook
               -- ^ url, events, enabled, secret
  deleteWebhook  :: Text -> m Bool                -- ^ id -> existed?

instance MonadWebhooks AppM where
  listWebhooks = do
    tv <- webhooksTVar
    sortOn (\w -> (whAddedAt w, whId w)) . HM.elems <$> liftIO (readTVarIO tv)

  getWebhook wid = do
    tv <- webhooksTVar
    liftIO (HM.lookup wid <$> readTVarIO tv)

  createWebhook url events enabled secret = do
    now <- liftIO getCurrentTime
    wid <- liftIO newWebhookId
    let w = Webhook wid url events enabled secret now
    _ <- modifyWebhooks (HM.insert wid w)
    pure w

  deleteWebhook wid = do
    m <- getWebhook wid
    case m of
      Nothing -> pure False
      Just _  -> do
        _ <- modifyWebhooks (HM.delete wid)
        pure True

-- | A fresh webhook id: 16 random hex chars (same minting style as the
-- collections/devices/auth ids).
newWebhookId :: IO Text
newWebhookId = do
  g <- newStdGen
  let rs = take 16 (randomRs (0, 15) g) :: [Int]
  pure (T.pack (map (\r -> "0123456789abcdef" !! r) rs))

-- | Module init: load the persisted @webhooks.json@ into a fresh TVar
-- under 'webhooksStateKey' (creating the state dir if needed).
loadWebhooksIntoState :: AppM ()
loadWebhooksIntoState = do
  cfg <- asks envConfig
  liftIO (createDirectoryIfMissing True (configStateDir cfg))
  loaded <- liftIO (loadWebhooks (webhooksFilePath cfg))
  tv <- liftIO (newTVarIO loaded)
  putState webhooksStateKey tv

-- Firing --------------------------------------------------------------------

-- | The JSON body POSTed to every matching webhook:
-- @{\"Event\": name, \"Data\": payload, \"Timestamp\": iso8601 now,
--   \"ServerId\": ...}@.
webhookBody :: Text -> Text -> Value -> UTCTime -> LBS.ByteString
webhookBody serverId event payload now =
  encode (object
    [ "Event"     .= event
    , "Data"      .= payload
    , "Timestamp" .= now
    , "ServerId"  .= serverId
    ])

-- | Fire @event@ with @payload@ at all enabled, subscribed webhooks
-- (milestone 12). Each delivery runs in its own forked thread (the caller
-- is NOT blocked); failures are logged via 'logErr' (ASCII) and skipped.
fireWebhooks :: Text -> Value -> AppM ()
fireWebhooks event payload = do
  env <- ask
  hooks <- listWebhooks
  serverId <- fromMaybe "unknown" <$> (getState "system.id" :: AppM (Maybe Text))
  now <- liftIO getCurrentTime
  let body = webhookBody serverId event payload now
      targets = [ w | w <- hooks, whEnabled w, eventMatches (whEvents w) event ]
  mapM_ (\w -> liftIO (forkIO (do
    ok <- deliverWebhook w body
    unless ok (void (runApp env (logErr ("webhook delivery failed: " <> whUrl w))))))) targets
  pure ()

-- | Synchronously POST @body@ to the webhook's URL (5s response timeout,
-- JSON content type, optional X-Webhook-Secret header). Returns True on a
-- 2xx response; any transport error / timeout / bad URL -> False. Never
-- throws.
deliverWebhook :: Webhook -> LBS.ByteString -> IO Bool
deliverWebhook w body = do
  manager <- newManager tlsManagerSettings
    { managerResponseTimeout = responseTimeoutMicro webhookTimeoutMicros }
  ereq <- try (parseRequest (T.unpack (whUrl w))) :: IO (Either SomeException Request)
  case ereq of
    Left _ -> pure False
    Right req0 -> do
      let req = req0
            { method = "POST"
            , requestBody = RequestBodyLBS body
            , requestHeaders = ("Content-Type", "application/json") : secretHeader
            }
          secretHeader = maybe [] (\s -> [("X-Webhook-Secret", TE.encodeUtf8 s)]) (whSecret w)
      eres <- try (httpLbs req manager) :: IO (Either SomeException (Response LBS.ByteString))
      pure $ case eres of
        Left _  -> False
        Right r -> let sc = statusCode (responseStatus r) in sc >= 200 && sc < 300

-- Internals ---------------------------------------------------------------

webhooksTVar :: AppM (TVar (HashMap Text Webhook))
webhooksTVar = do
  m <- getState webhooksStateKey
  case m of
    Just tv -> pure tv
    Nothing -> do
      tv <- liftIO (newTVarIO (HM.empty :: HashMap Text Webhook))
      putState webhooksStateKey tv
      pure tv

-- | Apply an update to the webhooks TVar AND persist to
-- @stateDir/webhooks.json@ after every mutation (the Stats pattern).
modifyWebhooks
  :: (HashMap Text Webhook -> HashMap Text Webhook)
  -> AppM (HashMap Text Webhook)
modifyWebhooks f = do
  cfg <- asks envConfig
  tv  <- webhooksTVar
  updated <- liftIO $ atomically $ do
    modifyTVar' tv f
    readTVar tv
  liftIO (saveWebhooks (webhooksFilePath cfg) updated)
  pure updated
