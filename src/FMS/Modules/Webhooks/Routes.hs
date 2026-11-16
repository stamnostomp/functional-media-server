module FMS.Modules.Webhooks.Routes
  ( webhooksRoutes
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), eitherDecode, encode, object, withObject, (.:), (.:?), (.!=), (.=))
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Network.HTTP.Types (status201)
import Network.Wai (Request, Response, responseLBS, strictRequestBody)

import FMS.Core.Module (Route, Segment (..), delete, get, jsonResponse, post)
import FMS.Core.Monad (AppError (..), AppM, getState)
import FMS.Modules.Webhooks.Monad
  ( MonadWebhooks (..), deliverWebhook, webhookBody )

-- | The webhook registry API (milestone 12, docs/todo.org section 12):
-- list/register/delete webhooks and fire a test event. Behind the normal
-- auth gate (strict mode requires a token); no extra restriction.
webhooksRoutes :: [Route]
webhooksRoutes =
  [ get    [Lit "Webhooks"] listWebhooksHandler
  , post   [Lit "Webhooks"] createWebhookHandler
  , delete [Lit "Webhooks", Capture "id"] deleteWebhookHandler
  , post   [Lit "Webhooks", Capture "id", Lit "Test"] testWebhookHandler
  ]

-- | Body of @POST /Webhooks@:
-- @{\"Url\": \"...\", \"Events\": [...], \"Enabled\"?: true, \"Secret\"?}@.
data CreateWebhookRequest = CreateWebhookRequest
  { cwrUrl     :: Text
  , cwrEvents  :: [Text]
  , cwrEnabled :: Bool
  , cwrSecret  :: Maybe Text
  }

instance FromJSON CreateWebhookRequest where
  parseJSON = withObject "CreateWebhookRequest" $ \o ->
    CreateWebhookRequest
      <$> o .:  "Url"
      <*> o .:? "Events"  .!= []
      <*> o .:? "Enabled" .!= True
      <*> o .:? "Secret"

-- Handlers ------------------------------------------------------------------

-- | @GET /Webhooks@ — 200 JSON array of all webhooks (oldest first).
listWebhooksHandler :: [(Text, Text)] -> Request -> AppM Response
listWebhooksHandler _captures _req =
  jsonResponse <$> listWebhooks

-- | @POST /Webhooks@ — register a webhook. 400 on a malformed body or an
-- empty @Url@; otherwise 201 with the new webhook's JSON.
createWebhookHandler :: [(Text, Text)] -> Request -> AppM Response
createWebhookHandler _captures req = do
  body <- liftIO (strictRequestBody req)
  case eitherDecode body of
    Left err -> throwError (BadRequest ("invalid JSON body: " <> T.pack err))
    Right cwr
      | T.null (T.strip (cwrUrl cwr)) ->
          throwError (BadRequest "webhook url must not be empty")
      | otherwise -> do
          w <- createWebhook (cwrUrl cwr) (cwrEvents cwr) (cwrEnabled cwr) (cwrSecret cwr)
          pure (responseLBS status201
                  [("Content-Type", "application/json")] (encode w))

-- | @DELETE /Webhooks/:id@ — 200 @{\"status\":\"ok\"@}, 404 when unknown.
deleteWebhookHandler :: [(Text, Text)] -> Request -> AppM Response
deleteWebhookHandler captures _req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just wid -> do
      ok <- deleteWebhook wid
      if ok
        then pure (jsonResponse (object ["status" .= ("ok" :: Text)]))
        else throwError (NotFound ("no webhook with id " <> wid))

-- | @POST /Webhooks/:id/Test@ — fire the event @\"Test\"@ at this webhook
-- SYNCHRONOUSLY (awaited, unlike 'fireWebhooks') and answer 200
-- @{\"delivered\": true|false}@; 404 when the webhook is unknown.
testWebhookHandler :: [(Text, Text)] -> Request -> AppM Response
testWebhookHandler captures _req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just wid -> do
      mW <- getWebhook wid
      case mW of
        Nothing -> throwError (NotFound ("no webhook with id " <> wid))
        Just w -> do
          serverId <- fromMaybe "unknown" <$> (getState "system.id" :: AppM (Maybe Text))
          now <- liftIO getCurrentTime
          let body = webhookBody serverId "Test" (object []) now
          ok <- liftIO (deliverWebhook w body)
          pure (jsonResponse (object ["delivered" .= ok]))
