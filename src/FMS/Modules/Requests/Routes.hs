module FMS.Modules.Requests.Routes
  ( requestsRoutes
  , CreateRequest (..)
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), eitherDecode, encode, object, withObject, (.:), (.:?), (.!=), (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Network.HTTP.Types (ResponseHeaders, status201, status204)
import Network.Wai (Response, queryString, responseLBS, strictRequestBody)
import qualified Network.Wai as Wai

import FMS.Core.Module (Route, Segment (..), delete, get, jsonResponse, post)
import FMS.Core.Monad (AppError (..), AppM)
import FMS.Core.Query (queryParams)
import FMS.Core.Types.User (User (..))
import FMS.Modules.Auth.Monad (currentUserFromReq, requireAdmin)
import FMS.Modules.Requests.Monad (MonadRequests (..), newRequestId)
import FMS.Modules.Requests.Types (Request (..), RequestStatus (..), duplicateOf, isActiveStatus)

jsonCT :: ResponseHeaders
jsonCT = [("Content-Type", "application/json")]

-- | The media-request queue API (m20a CRUD core; Radarr/Sonarr-style).
-- All routes go through the normal auth gate (any authenticated user in
-- strict mode); Approve/Reject are admin-only; reads are per-user unless
-- admin. Literal routes first, captures after (first-match-wins).
requestsRoutes :: [Route]
requestsRoutes =
  [ get    [Lit "Requests"] listRequestsHandler
  , post   [Lit "Requests"] createRequestHandler
  , get    [Lit "Requests", Capture "id"] getRequestHandler
  , post   [Lit "Requests", Capture "id", Lit "Approve"] (transitionHandler Approved)
  , post   [Lit "Requests", Capture "id", Lit "Reject"] (transitionHandler Rejected)
  , delete [Lit "Requests", Capture "id"] deleteRequestHandler
  ]

-- | Body of @POST /Requests@ (PascalCase, Jellyfin-ish).
data CreateRequest = CreateRequest
  { crTitle    :: Text
  , crYear     :: Maybe Int
  , crType     :: Text
  , crQuery    :: Maybe Text
  , crSizeBytes :: Maybe Integer
  , crIndexer  :: Maybe Text
  , crRelease  :: Maybe Text
  , crSourceUrl :: Maybe Text
  }

instance FromJSON CreateRequest where
  parseJSON = withObject "CreateRequest" $ \o ->
    CreateRequest
      <$> o .:  "Title"
      <*> o .:? "Year"
      <*> o .:? "Type" .!= "movie"
      <*> o .:? "Query"
      <*> o .:? "SizeBytes"
      <*> o .:? "Indexer"
      <*> o .:? "Release"
      <*> o .:? "SourceUrl"

-- Handlers ------------------------------------------------------------------

-- | @GET /Requests@ -> @{\"Requests\": [...], \"TotalRecordCount\": n}@.
-- Non-admins see only their own requests; admins see all. The optional
-- @userId@ filter is admin-only (403 otherwise).
listRequestsHandler :: [(Text, Text)] -> Wai.Request -> AppM Response
listRequestsHandler _captures req = do
  mUser <- currentUserFromReq req
  all' <- listRequests
  let isAdminUser = maybe True isAdmin mUser  -- open mode (no user) sees all
      qparams = queryParams (queryString req)
  filtered <- case lookup "userid" qparams of
    Just uid
      | isAdminUser -> pure (filter (\r -> reqUserId r == uid) all')
      | otherwise   -> throwError (Forbidden "userId filter is admin-only")
    Nothing
      | isAdminUser -> pure all'
      | otherwise   -> case mUser of
          Just u  -> pure (filter (\r -> reqUserId r == userId u) all')
          Nothing -> pure []
  pure (jsonResponse (object
    [ "Requests" .= filtered
    , "TotalRecordCount" .= length filtered
    ]))

-- | @GET /Requests/:id@ — owner or admin (403 otherwise), 404 unknown.
getRequestHandler :: [(Text, Text)] -> Wai.Request -> AppM Response
getRequestHandler captures req = do
  rid <- captureId captures
  mUser <- currentUserFromReq req
  m <- getRequest rid
  case m of
    Nothing -> throwError (NotFound ("no request with id " <> rid))
    Just r  -> do
      case mUser of
        Just u | not (isAdmin u) && userId u /= reqUserId r ->
          throwError (Forbidden "not your request")
        _ -> pure (jsonResponse r)

-- | @POST /Requests@ — create a request for the current user (201 with the
-- created request). Title must be non-empty (400). Duplicate check across
-- ALL users: an active request with the same 'normalizeTitle' -> 409 with
-- the unified error body, naming the existing request id.
createRequestHandler :: [(Text, Text)] -> Wai.Request -> AppM Response
createRequestHandler _captures req = do
  body <- liftIO (strictRequestBody req)
  cr <- case eitherDecode body of
    Left err -> throwError (BadRequest ("invalid JSON body: " <> T.pack err))
    Right v  -> pure v
  if T.strip (crTitle cr) == ""
    then throwError (BadRequest "Title must be non-empty")
    else pure ()
  mUser <- currentUserFromReq req
  all' <- listRequests
  case duplicateOf (crTitle cr) all' of
    Just existing ->
      throwError (Conflict ("already in progress (request " <> reqId existing <> ")"))
    Nothing -> do
      now <- liftIO getCurrentTime
      rid <- liftIO newRequestId
      let (uid, uname) = case mUser of
            Just u  -> (userId u, userName u)
            Nothing -> ("", "")
          r = Request
            { reqId = rid
            , reqUserId = uid
            , reqUserName = uname
            , reqTitle = crTitle cr
            , reqYear = crYear cr
            , reqMediaType = crType cr
            , reqQuery = maybe (crTitle cr) id (crQuery cr)
            , reqStatus = Pending
            , reqSizeBytes = crSizeBytes cr
            , reqSourceIndexer = crIndexer cr
            , reqSourceRelease = crRelease cr
            , reqSourceUrl = crSourceUrl cr
            , reqCreatedAt = now
            , reqUpdatedAt = now
            , reqApprovedBy = Nothing
            , reqError = Nothing
            }
      _ <- insertRequest r
      pure (responseLBS status201 jsonCT (encode r))

-- | @POST /Requests/:id/Approve@ and @.../Reject@ (admin only): transition
-- from Pending to the given status (400 otherwise), bumping UpdatedAt;
-- Approve also records the approving admin. 200 with the updated request.
transitionHandler :: RequestStatus -> [(Text, Text)] -> Wai.Request -> AppM Response
transitionHandler target captures req = do
  _ <- requireAdmin req
  mAdmin <- currentUserFromReq req
  rid <- captureId captures
  m <- getRequest rid
  case m of
    Nothing -> throwError (NotFound ("no request with id " <> rid))
    Just r
      | reqStatus r /= Pending ->
          throwError (BadRequest "only pending requests can be approved/rejected")
      | otherwise -> do
          m' <- setRequestStatus rid target (userName <$> mAdmin)
          case m' of
            Just r' -> pure (jsonResponse r')
            Nothing -> throwError (NotFound ("no request with id " <> rid))

-- | @DELETE /Requests/:id@ — owner or admin; only while the request is
-- still active (Pending/Approved/Downloading), else 400. 204 on success,
-- 404 unknown.
deleteRequestHandler :: [(Text, Text)] -> Wai.Request -> AppM Response
deleteRequestHandler captures req = do
  rid <- captureId captures
  mUser <- currentUserFromReq req
  m <- getRequest rid
  case m of
    Nothing -> throwError (NotFound ("no request with id " <> rid))
    Just r -> do
      case mUser of
        Just u | not (isAdmin u) && userId u /= reqUserId r ->
          throwError (Forbidden "not your request")
        _ -> pure ()
      if not (isActiveStatus (reqStatus r))
        then throwError (BadRequest "only active requests can be cancelled")
        else do
          _ <- deleteRequest rid
          pure (responseLBS status204 [] "")

-- Internals -----------------------------------------------------------------

captureId :: [(Text, Text)] -> AppM Text
captureId captures =
  case lookup "id" captures of
    Just i  -> pure i
    Nothing -> throwError (BadRequest "missing id capture")
