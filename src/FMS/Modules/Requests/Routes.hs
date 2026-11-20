module FMS.Modules.Requests.Routes
  ( requestsRoutes
  , CreateRequest (..)
  , markImported
  , fetchTorrentStatuses
  , attachLiveProgress
  ) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException, try)
import Data.List (isPrefixOf)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ask, asks)
import Data.Aeson (FromJSON (..), eitherDecode, encode, object, withObject, (.:), (.:?), (.!=), (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Network.HTTP.Types (ResponseHeaders, status201, status204)
import Network.Wai (Response, queryString, responseLBS, strictRequestBody)
import qualified Network.Wai as Wai


import FMS.Core.Module (Route, Segment (..), delete, get, jsonResponse, post)
import FMS.Core.Monad (AppError (..), AppM, MonadLog (..), envConfig, runApp)
import FMS.Modules.Indexers (resolveReleaseBytes)
import FMS.Core.Query (queryParams)
import FMS.Core.Types.Config (Config (..))
import FMS.Core.Types.MediaItem (MediaItem (..))
import FMS.Core.Types.User (User (..))
import FMS.Modules.Auth.Monad (currentUserFromReq, requireAdmin)
import FMS.Modules.Library.Monad (MonadLibrary (..))
import FMS.Modules.Library.Scan (scanDirectory)
import FMS.Modules.Metadata.Api.Tmdb (tmdbMovieByIdAt, tmdbMovieDetailsAt)
import FMS.Modules.Metadata.Monad (MonadMetadata (..), getMetadataSettings, tmdbResultToMetadata)
import FMS.Modules.Metadata.Types (MediaMetadata (..), msTmdbApiKey)
import FMS.Modules.Requests.Monad (MonadRequests (..), getRequestSettings, newRequestId, updateRequestSettings)
import FMS.Modules.AnnounceTrackers (currentAnnounceTrackers)
import FMS.Modules.Requests.Push (autoApproveSize, delugeLoginTest, delugeTorrentStatuses, importComplete, matchTorrentProgress, matchesRequestDownload, parseProxiedPath, pushRequest)
import FMS.Modules.Requests.Types (Request (..), RequestSettings (..), RequestStatus (..), SettingsPatch (..), duplicateOf, effectiveAutoApproveGb, isActiveStatus, mergeSettings, settingsPublicJson)

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
    -- m27: settings routes are literals, registered BEFORE the id capture
    -- (first-match-wins, so /Requests/Settings is not swallowed by :id)
  , get    [Lit "Requests", Lit "Settings"] getSettingsHandler
  , post   [Lit "Requests", Lit "Settings"] postSettingsHandler
  , post   [Lit "Requests", Lit "Settings", Lit "Test"] testSettingsHandler
  , get    [Lit "Requests", Capture "id"] getRequestHandler
  , post   [Lit "Requests", Capture "id", Lit "Approve"] (transitionHandler Approved)
  , post   [Lit "Requests", Capture "id", Lit "Reject"] (transitionHandler Rejected)
  , delete [Lit "Requests", Capture "id"] deleteRequestHandler
  ]

-- | Body of @POST /Requests@ (PascalCase, Jellyfin-ish).
data CreateRequest = CreateRequest
  { crTitle    :: Text
  , crYear     :: Maybe Int
  , crTmdbId   :: Maybe Int      -- ^ m68: TMDB id from the Discover card
  , crType     :: Text
  , crQuery    :: Maybe Text
  , crSizeBytes :: Maybe Integer
  , crIndexer  :: Maybe Text
  , crRelease  :: Maybe Text
  , crSourceUrl :: Maybe Text
  , crTrackerId :: Maybe Text
  , crDownloadUrl :: Maybe Text
  , crMagnet   :: Maybe Text
  }

instance FromJSON CreateRequest where
  parseJSON = withObject "CreateRequest" $ \o ->
    CreateRequest
      <$> o .:  "Title"
      <*> o .:? "Year"
      <*> o .:? "TmdbId"
      <*> o .:? "Type" .!= "movie"
      <*> o .:? "Query"
      <*> o .:? "SizeBytes"
      <*> o .:? "Indexer"
      <*> o .:? "Release"
      <*> o .:? "SourceUrl"
      <*> o .:? "TrackerId"
      <*> o .:? "DownloadUrl"
      <*> o .:? "Magnet"

-- Handlers ------------------------------------------------------------------

-- | m20b: mark Downloading requests as Imported when a media file whose
-- (extension-stripped) name normalizes to the request title appears in
-- the download dir. Persists the status change. m30: the download dir is
-- walked RECURSIVELY (Deluge saves into <downloads>/<Torrent.Name>/...);
-- 'scanDirectory' also filters isMediaFile and returns [] for a missing
-- dir.
markImported :: [(Text, Text, Double, Double)] -> [Request] -> AppM ()
markImported statuses reqs = do
  s <- getRequestSettings   -- m27: effective (saved) download dir
  let dir = T.unpack (rsDownloadDir s)
  efiles <- liftIO (try (scanDirectory False dir) :: IO (Either SomeException [FilePath]))
  let files = either (const []) id efiles
  mapM_ markOne [ r | r <- reqs, reqStatus r == Downloading
                     , matchesRequestDownload (reqTitle r) (reqSourceRelease r) files
                     , importComplete statuses (reqTitle r) (reqSourceRelease r) ]  -- m38: only when the torrent is complete
  where
    markOne r = do
      _ <- setRequestStatus (reqId r) Imported Nothing
      applyRequestTmdb r   -- m68: resolve metadata from the request's TMDB id when present
      pure ()

-- | m68: when a request carries a TMDB id (Discover card), the content is
-- known exactly -- set the imported library item's metadata straight from
-- @\/movie\/<id>@, no name sanitizing/searching. When there is no id (or
-- the fetch fails) nothing happens here and the normal scan-time
-- name-based TMDB refresh remains as the fallback.
applyRequestTmdb :: Request -> AppM ()
applyRequestTmdb r =
  case reqTmdbId r of
    Nothing -> pure ()
    Just tid
      | T.toLower (reqMediaType r) == "tv" -> pure ()  -- episode/season requests keep the name-based path
      | otherwise -> do
          cfg <- asks envConfig
          ms <- getMetadataSettings
          if T.null (msTmdbApiKey ms)
            then pure ()
            else do
              s <- getRequestSettings
              let dir = T.unpack (rsDownloadDir s)
              items <- listItems
              let cands = [ i | i <- items
                      , not (null dir)
                      , dir `isPrefixOf` itemPath i
                      , matchesRequestDownload (reqTitle r) (reqSourceRelease r) [itemPath i] ]
              case cands of
                [] -> pure ()
                (i : _) -> do
                  eres <- liftIO (tmdbMovieByIdAt (configTmdbBaseUrl cfg) (msTmdbApiKey ms) tid)
                  case eres of
                    Left err ->
                      logWarn ("requests: tmdb /movie/" <> T.pack (show tid)
                               <> " failed: " <> T.pack err)
                    Right top -> do
                      dres <- liftIO (tmdbMovieDetailsAt (configTmdbBaseUrl cfg) (msTmdbApiKey ms) tid)
                      let (gs, kws) = either (const ([], [])) id dres
                          fresh = (tmdbResultToMetadata top)
                            { metaGenres = gs, metaKeywords = kws }
                      setMetadata (itemId i) fresh
                      logInfo ("requests: import metadata for \"" <> itemTitle i
                               <> "\" set from TMDB id " <> T.pack (show tid))

-- | m38: fetch live Deluge torrent statuses ONCE per read (shared by
-- 'markImported' and 'attachLiveProgress'); graceful [] on any failure.
fetchTorrentStatuses :: AppM [(Text, Text, Double, Double)]
fetchTorrentStatuses = do
  s <- getRequestSettings
  est <- liftIO (delugeTorrentStatuses (rsDelugeUrl s) (rsDelugePassword s))
  case est of
    Left err -> do
      logWarn ("requests: deluge status query failed: " <> T.pack err)
      pure []
    Right sts -> pure sts

-- | m37: fill live Deluge progress/state onto requests whose title/release
-- matches a live torrent by querying the Deluge web API
-- (core.get_torrents_status) with the effective settings. m50: ANY request
-- status is filled (not only Downloading), so an Imported-but-still-seeding
-- torrent keeps Progress 100 / TorrentState Seeding. Failure-graceful: on
-- any error the fields stay Nothing (null).
attachLiveProgress :: [(Text, Text, Double, Double)] -> [Request] -> [Request]
attachLiveProgress statuses reqs = map (fillOne statuses) reqs
  where
    fillOne sts r = case matchTorrentProgress (reqTitle r) (reqSourceRelease r) sts of
      Just (p, st) -> r { reqProgress = Just p, reqTorrentState = Just st }
      Nothing      -> r

-- | @GET /Requests@ -> @{\"Requests\": [...], \"TotalRecordCount\": n}@.
-- Non-admins see only their own requests; admins see all. The optional
-- @userId@ filter is admin-only (403 otherwise). m20b: Downloading
-- requests are first checked against the download dir (auto-import).
listRequestsHandler :: [(Text, Text)] -> Wai.Request -> AppM Response
listRequestsHandler _captures req = do
  mUser <- currentUserFromReq req
  all0 <- listRequests
  sts <- fetchTorrentStatuses       -- m38: one Deluge query per read
  markImported sts all0
  all' <- listRequests
  let all'' = attachLiveProgress sts all'   -- m37: live Deluge progress on Downloading rows
  let isAdminUser = maybe True isAdmin mUser  -- open mode (no user) sees all
      qparams = queryParams (queryString req)
  filtered <- case lookup "userid" qparams of
    Just uid
      | isAdminUser -> pure (filter (\r -> reqUserId r == uid) all'')
      | otherwise   -> throwError (Forbidden "userId filter is admin-only")
    Nothing
      | isAdminUser -> pure all''
      | otherwise   -> case mUser of
          Just u  -> pure (filter (\r -> reqUserId r == userId u) all'')
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
  all0 <- listRequests
  sts <- fetchTorrentStatuses       -- m38: one Deluge query per read
  markImported sts all0
  m <- getRequest rid
  case m of
    Nothing -> throwError (NotFound ("no request with id " <> rid))
    Just r  -> do
      case mUser of
        Just u | not (isAdmin u) && userId u /= reqUserId r ->
          throwError (Forbidden "not your request")
        _ -> pure (jsonResponse (maybe r id (listToMaybe' (attachLiveProgress sts [r]))))
  where
    listToMaybe' [] = Nothing
    listToMaybe' (x : _) = Just x

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
      cfg <- asks envConfig
      s <- getRequestSettings   -- m27: effective settings (saved win over env)
      let (uid, uname) = case mUser of
            Just u  -> (userId u, userName u)
            Nothing -> ("", "")
          threshold = round (effectiveAutoApproveGb cfg s * 1024 * 1024 * 1024) :: Integer
          autoAppr = autoApproveSize (crSizeBytes cr) threshold
          r0 = Request
            { reqId = rid
            , reqUserId = uid
            , reqUserName = uname
            , reqTitle = crTitle cr
            , reqYear = crYear cr
            , reqTmdbId = crTmdbId cr   -- m68
            , reqMediaType = crType cr
            , reqQuery = maybe (crTitle cr) id (crQuery cr)
            , reqStatus = if autoAppr then Approved else Pending
            , reqSizeBytes = crSizeBytes cr
            , reqSourceIndexer = crIndexer cr <|> crTrackerId cr
            , reqSourceRelease = crRelease cr
            , reqSourceUrl = crSourceUrl cr
            , reqMagnet = crMagnet cr
            , reqDownloadUrl = crDownloadUrl cr <|> crSourceUrl cr
            , reqCreatedAt = now
            , reqUpdatedAt = now
            , reqApprovedBy = Nothing
            , reqError = Nothing
            , reqDownloadDir = Just (rsDownloadDir s)   -- m27: stamp the location
            , reqProgress = Nothing      -- m37: live-only, filled on read
            , reqTorrentState = Nothing  -- m37: live-only, filled on read
            }
      _ <- insertRequest r0
      -- m20b: auto-approved requests are pushed immediately
      r1 <- if autoAppr then pushAndAdvance r0 else pure r0
      pure (responseLBS status201 jsonCT (encode r1))

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
            Just r' -> do
              -- m20b: an approved request with a download source is pushed
              r'' <- if target == Approved then pushAndAdvance r' else pure r'
              pure (jsonResponse r'')
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

-- | Push the request's release to Deluge (or the download-dir fallback)
-- and advance the status to Downloading, or Failed with the error text.
pushAndAdvance :: Request -> AppM Request
pushAndAdvance r = do
  env <- ask
  s <- getRequestSettings   -- m27: effective (saved) deluge url/password/dir
  -- m21 fix: same-origin /Indexers/:id/Download?r=... URLs are resolved
  -- in-process (the HTTP route is auth-gated, so a loopback fetch would
  -- 401 under FMS_REQUIRE_AUTH=true)
  let resolveProxied path = case parseProxiedPath path of
        Nothing -> pure (Left "not a proxied indexer path")
        Just (iid, rid) -> do
          ea <- runApp env (resolveReleaseBytes iid rid)
          pure (either (Left . show) (either Left (Right . snd)) ea)
  -- m62: attach the refreshed public announce-tracker list to the Deluge add
  trackers <- currentAnnounceTrackers
  eres <- liftIO (pushRequest (rsDelugeUrl s) (rsDelugePassword s)
                (T.unpack (rsDownloadDir s)) (reqMagnet r) (reqDownloadUrl r) (reqTitle r)
                trackers resolveProxied)
  case eres of
    Right () -> do
      m <- setRequestStatus (reqId r) Downloading Nothing
      pure (maybe r id m)
    Left err -> do
      m <- setRequestFailed (reqId r) (T.pack err)
      pure (maybe r id m)

-- m27: download-client settings ---------------------------------------------

-- | @GET /Requests/Settings@ — any authenticated user; the Deluge password
-- is redacted (null) for non-admins (same pattern as indexer ApiKey).
getSettingsHandler :: [(Text, Text)] -> Wai.Request -> AppM Response
getSettingsHandler _captures req = do
  mUser <- currentUserFromReq req
  s <- getRequestSettings
  pure (jsonResponse (settingsPublicJson (maybe False isAdmin mUser) s))

-- | @POST /Requests/Settings@ — admin only (403 otherwise). Accepts any
-- subset of {DelugeUrl, DelugePassword, DownloadDir, AutoApproveGb}
-- (PascalCase or camelCase), merges into the saved settings, persists, and
-- returns the full settings object (the caller is admin, so unredacted).
postSettingsHandler :: [(Text, Text)] -> Wai.Request -> AppM Response
postSettingsHandler _captures req = do
  _ <- requireAdmin req
  body <- liftIO (strictRequestBody req)
  patch <- case eitherDecode body of
    Left err -> throwError (BadRequest ("invalid JSON body: " <> T.pack err))
    Right v  -> pure v
  -- validation: urls must be http(s), auto-approve must be >= 0
  case spDelugeUrl patch of
    Just u | not ("http://" `T.isPrefixOf` u || "https://" `T.isPrefixOf` u) ->
      throwError (BadRequest "DelugeUrl must be an http(s) URL")
    _ -> pure ()
  case spAutoApproveGb patch of
    Just (Just gb) | gb < 0 -> throwError (BadRequest "AutoApproveGb must be >= 0")
    _ -> pure ()
  s <- updateRequestSettings (\old -> mergeSettings old patch)
  pure (jsonResponse (settingsPublicJson True s))

-- | @POST /Requests/Settings/Test@ — admin only. Optionally accepts a
-- settings body to test (not persisted); otherwise tests the effective
-- saved settings with a bare Deluge auth.login. Always 200 with
-- {ok:true} or {ok:false, error} — never 500.
testSettingsHandler :: [(Text, Text)] -> Wai.Request -> AppM Response
testSettingsHandler _captures req = do
  _ <- requireAdmin req
  body <- liftIO (strictRequestBody req)
  mPatch <- case eitherDecode body of
    Left _   -> pure Nothing   -- empty/invalid body: test the saved settings
    Right (p :: SettingsPatch) -> pure (Just p)
  base <- getRequestSettings
  let s = maybe base (\p -> mergeSettings base p) mPatch
  eres <- liftIO (delugeLoginTest (rsDelugeUrl s) (rsDelugePassword s))
  pure $ jsonResponse $ case eres of
    Right () -> object [ "Ok" .= True ]
    Left err -> object [ "Ok" .= False, "Error" .= T.pack err ]

captureId :: [(Text, Text)] -> AppM Text
captureId captures =
  case lookup "id" captures of
    Just i  -> pure i
    Nothing -> throwError (BadRequest "missing id capture")
