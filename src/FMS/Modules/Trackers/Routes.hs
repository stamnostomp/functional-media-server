module FMS.Modules.Trackers.Routes
  ( trackersRoutes
  , CreateTracker (..)
  ) where

import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), eitherDecode, encode, object, withObject, (.:), (.:?), (.!=), (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types (ResponseHeaders, status201, status204)
import Network.Wai (Response, queryString, responseLBS, strictRequestBody)
import qualified Network.Wai as Wai

import FMS.Core.Module (Route, Segment (..), delete, get, jsonResponse, post)
import FMS.Core.Monad (AppError (..), AppM)
import FMS.Core.Query (queryParams)
import FMS.Core.Types.User (User (..))
import FMS.Modules.Auth.Monad (currentUserFromReq, requireAdmin)
import FMS.Modules.Trackers.Monad (MonadTrackers (..))
import FMS.Modules.Trackers.Torznab (jackettSearch, jackettSearchUrl)
import FMS.Modules.Trackers.Types (Release (..), Tracker (..), trackerPublicJson)

jsonCT :: ResponseHeaders
jsonCT = [("Content-Type", "application/json")]

-- | The tracker (Jackett indexer) API (m20b). Reads are open to any
-- authenticated user but REDACT the ApiKey for non-admins; mutations are
-- admin-only. Literals before captures (first-match-wins).
trackersRoutes :: [Route]
trackersRoutes =
  [ get    [Lit "Trackers"] listTrackersHandler
  , post   [Lit "Trackers"] createTrackerHandler
  , get    [Lit "Trackers", Capture "id"] getTrackerHandler
  , delete [Lit "Trackers", Capture "id"] deleteTrackerHandler
  , post   [Lit "Trackers", Capture "id", Lit "Enable"] (setEnabledHandler True)
  , post   [Lit "Trackers", Capture "id", Lit "Disable"] (setEnabledHandler False)
  , get    [Lit "Trackers", Capture "id", Lit "Search"] searchHandler
  ]

-- | Body of @POST /Trackers@ (admin).
data CreateTracker = CreateTracker
  { ctName    :: Text
  , ctUrl     :: Text
  , ctApiKey  :: Text
  , ctEnabled :: Bool
  }

instance FromJSON CreateTracker where
  parseJSON = withObject "CreateTracker" $ \o ->
    CreateTracker
      <$> o .:  "Name"
      <*> o .:  "Url"
      <*> o .:? "ApiKey" .!= ""
      <*> o .:? "Enabled" .!= True

-- Handlers ------------------------------------------------------------------

isAdminReq :: Wai.Request -> AppM Bool
isAdminReq req = do
  mUser <- currentUserFromReq req
  pure (maybe False isAdmin mUser)

-- | @GET /Trackers@ — any authenticated user; ApiKey redacted (null) for
-- non-admins.
listTrackersHandler :: [(Text, Text)] -> Wai.Request -> AppM Response
listTrackersHandler _captures req = do
  admin <- isAdminReq req
  ts <- listTrackers
  pure (jsonResponse (map (trackerPublicJson admin) ts))

-- | @GET /Trackers/:id@ — same redaction rule; 404 unknown.
getTrackerHandler :: [(Text, Text)] -> Wai.Request -> AppM Response
getTrackerHandler captures req = do
  admin <- isAdminReq req
  tid <- captureId captures
  m <- getTracker tid
  case m of
    Nothing -> throwError (NotFound ("no tracker with id " <> tid))
    Just t  -> pure (jsonResponse (trackerPublicJson admin t))

-- | @POST /Trackers@ (admin) — 201 with the created tracker (full JSON,
-- the creator is admin). Url must be http(s).
createTrackerHandler :: [(Text, Text)] -> Wai.Request -> AppM Response
createTrackerHandler _captures req = do
  _ <- requireAdmin req
  body <- liftIO (strictRequestBody req)
  ct <- case eitherDecode body of
    Left err -> throwError (BadRequest ("invalid JSON body: " <> T.pack err))
    Right v  -> pure v
  let u = T.toLower (ctUrl ct)
  unless (T.isPrefixOf "http://" u || T.isPrefixOf "https://" u)
    (throwError (BadRequest "Url must start with http:// or https://"))
  t <- addTracker (ctName ct) (ctUrl ct) (ctApiKey ct) (ctEnabled ct)
  pure (responseLBS status201 jsonCT (encode t))

-- | @DELETE /Trackers/:id@ (admin) — 204, 404 unknown.
deleteTrackerHandler :: [(Text, Text)] -> Wai.Request -> AppM Response
deleteTrackerHandler captures req = do
  _ <- requireAdmin req
  tid <- captureId captures
  ok <- deleteTracker tid
  if not ok
    then throwError (NotFound ("no tracker with id " <> tid))
    else pure (responseLBS status204 [] "")

-- | @POST /Trackers/:id/Enable|Disable@ (admin) — 200 with the updated
-- tracker (full JSON; caller is admin), 404 unknown.
setEnabledHandler :: Bool -> [(Text, Text)] -> Wai.Request -> AppM Response
setEnabledHandler enabled captures req = do
  _ <- requireAdmin req
  tid <- captureId captures
  ok <- setTrackerEnabled tid enabled
  if not ok
    then throwError (NotFound ("no tracker with id " <> tid))
    else do
      m <- getTracker tid
      case m of
        Just t  -> pure (jsonResponse t)
        Nothing -> throwError (NotFound ("no tracker with id " <> tid))

-- | @GET /Trackers/:id/Search?q=&type=movie|tv@ — any authenticated user;
-- only enabled trackers (404 unknown, 400 disabled). Calls the Jackett
-- Torznab endpoint and responds with the parsed releases (TrackerId added).
searchHandler :: [(Text, Text)] -> Wai.Request -> AppM Response
searchHandler captures req = do
  tid <- captureId captures
  m <- getTracker tid
  case m of
    Nothing -> throwError (NotFound ("no tracker with id " <> tid))
    Just t
      | not (trackerEnabled t) -> throwError (BadRequest "tracker is disabled")
      | otherwise -> do
          let qparams = queryParams (queryString req)
          q <- case lookup "q" qparams of
            Just v | not (T.null (T.strip v)) -> pure v
            _ -> throwError (BadRequest "missing q parameter")
          let mType = lookup "type" qparams
          eres <- liftIO (jackettSearch (jackettSearchUrl (trackerUrl t) tid (trackerApiKey t) q mType))
          case eres of
            Left err -> throwError (ServiceUnavailable (T.pack err))
            Right rels ->
              pure (jsonResponse
                [ object [ "Title" .= relTitle r, "SizeBytes" .= relSizeBytes r
                         , "Seeders" .= relSeeders r, "DownloadUrl" .= relDownloadUrl r
                         , "Magnet" .= relMagnet r, "TrackerId" .= tid ]
                | r <- rels ])

-- Internals -----------------------------------------------------------------

captureId :: [(Text, Text)] -> AppM Text
captureId captures =
  case lookup "id" captures of
    Just i  -> pure i
    Nothing -> throwError (BadRequest "missing id capture")
