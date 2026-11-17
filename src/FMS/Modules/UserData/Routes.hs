module FMS.Modules.UserData.Routes
  ( userDataRoutes
  , parseIdsBody
  ) where

import Control.Applicative ((<|>))
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), ToJSON (..), Value, eitherDecode, object, withObject, (.:), (.:?), (.!=), (.=))
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString.Lazy as LBS
import Data.CaseInsensitive (original)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Network.Wai (Request, Response, queryString, requestHeaders, strictRequestBody)

import FMS.Core.Module (Route, Segment (..), delete, get, jsonResponse, post)
import FMS.Core.Monad (AppError (..), AppM)
import FMS.Core.Query (latestItems, queryParams, viewsFromFolders)
import FMS.Core.Router (parseTokenHeader)
import FMS.Core.Types.MediaFolder (MediaFolder (..))
import FMS.Core.Types.MediaItem (MediaItem)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import FMS.Core.Types.User (User (..))
import FMS.Modules.Auth.Monad (MonadAuth (..), allowedFoldersFor, canAccessFolder, visibleItems)
import FMS.Modules.Library (MonadLibrary (..))
-- FMS.Modules.Library.Routes imports only UserData.Monad/Types (not this
-- module), so importing it here does not create a cycle.
import FMS.Modules.Library.Routes (itemsQueryResponse)
import FMS.Modules.Socket (broadcastSocket)
import FMS.Modules.Webhooks (fireWebhooks)
import FMS.Modules.UserData.Monad
  ( MonadUserData (..)
  , getDisplayPrefs
  , markPlayed
  , setDisplayPrefs
  , setFavorite
  )
import FMS.Modules.UserData.Types (UserData (..))

-- | The per-user item data API (docs/todo.org section 9, milestone 5c),
-- Jellyfin-shaped. Every route requires a valid credential
-- (@X-Emby-Token@ / @Authorization@, parsed with 'parseTokenHeader' and
-- resolved with 'MonadAuth'), and the @:userId@ in the path must be the
-- token's own user or the token must be an admin — otherwise 401. The
-- @:itemId@ must exist in the library (404 otherwise).
userDataRoutes :: [Route]
userDataRoutes =
  -- Milestone 5f: the 3-segment literal routes come first (first match
  -- wins) so they are not shadowed by the 4/5-segment capture routes.
  [ get    [Lit "Users", Capture "userId", Lit "Views"] viewsHandler
  , get    [Lit "Users", Capture "userId", Lit "Items"] usersItemsHandler
  , get    [Lit "Users", Capture "userId", Lit "Items", Lit "Latest"] latestItemsHandler
  , get    [Lit "DisplayPreferences"] displayPrefsGetHandler
  , post   [Lit "DisplayPreferences"] displayPrefsPostHandler
  , delete [Lit "Users", Capture "userId", Lit "PlayedItems"]   batchUnplayedHandler
  , delete [Lit "Users", Capture "userId", Lit "FavoriteItems"] batchUnfavoriteHandler
  , get    [Lit "Users", Capture "userId", Lit "Items", Capture "itemId", Lit "UserData"]
           getUserDataHandler
  , post   [Lit "Users", Capture "userId", Lit "Items", Capture "itemId", Lit "UserData"]
           updateUserDataHandler
  , post   [Lit "Users", Capture "userId", Lit "PlayedItems", Capture "itemId"]
           playedHandler
  , delete [Lit "Users", Capture "userId", Lit "PlayedItems", Capture "itemId"]
           unplayedHandler
  , post   [Lit "Users", Capture "userId", Lit "FavoriteItems", Capture "itemId"]
           favoriteHandler
  , delete [Lit "Users", Capture "userId", Lit "FavoriteItems", Capture "itemId"]
           unfavoriteHandler
  ]

-- | Body of the milestone-5f batch deletes:
-- @{\"ItemIds\": [...]}@ (Jellyfin) or @{\"Ids\": [...]}@.
data IdsBody = IdsBody { idsBodyIds :: [Text] }

instance FromJSON IdsBody where
  parseJSON = withObject "IdsBody" $ \o ->
    IdsBody <$> (o .: "ItemIds" <|> (o .:? "Ids" .!= []))

-- | Pure batch-body parser (exported for tests): decode the JSON body and
-- return the id list; 'Left' on malformed JSON.
parseIdsBody :: LBS.ByteString -> Either String [Text]
parseIdsBody bs = idsBodyIds <$> eitherDecode bs

-- | Body of @POST /Users/:userId/PlayedItems/:itemId@ (Jellyfin field name).
data PlayedRequest = PlayedRequest
  { playedReqPlayed :: Bool
  }

instance FromJSON PlayedRequest where
  parseJSON = withObject "PlayedRequest" $ \o ->
    PlayedRequest <$> o .: "Played"

-- Handlers ------------------------------------------------------------------

-- | @GET /Users/:userId/Items/:itemId/UserData@ — the stored record, or the
-- empty default when nothing is stored yet. 200 with the 'UserData' JSON.
getUserDataHandler :: [(Text, Text)] -> Request -> AppM Response
getUserDataHandler captures req = do
  (uid, iid) <- captureUserItem captures
  _ <- requireUserDataAccess req uid
  _ <- requireExistingItem iid
  ud <- getUserData uid iid
  pure (jsonResponse ud)

-- | @POST /Users/:userId/Items/:itemId/UserData@ with a partial JSON body
-- (any subset of @played@ / @favorite@ / @playbackPosition@ / @playCount@ /
-- @lastPlayedAt@). The present fields are merged into the stored record
-- (merge, not replace), persisted, and the full updated record is
-- returned with 200. Malformed JSON or a wrongly typed field -> 400.
updateUserDataHandler :: [(Text, Text)] -> Request -> AppM Response
updateUserDataHandler captures req = do
  (uid, iid) <- captureUserItem captures
  _ <- requireUserDataAccess req uid
  _ <- requireExistingItem iid
  body <- liftIO (strictRequestBody req)
  case eitherDecode body of
    Left err -> throwError (BadRequest ("invalid JSON body: " <> T.pack err))
    Right patch -> do
      existing <- getUserData uid iid
      case mergeUserDataPatch patch existing of
        Left err -> throwError (BadRequest ("invalid UserData field: " <> T.pack err))
        Right merged -> do
          ud <- updateUserData uid iid (const merged)
          _ <- broadcastSocket "UserDataSaved" (object   -- milestone 8
                 [ "UserId"   .= uid
                 , "ItemId"   .= iid
                 , "UserData" .= toJSON ud
                 ])
          fireWebhooks "UserDataSaved" (object   -- milestone 12
                 [ "UserId"   .= uid
                 , "ItemId"   .= iid
                 , "UserData" .= toJSON ud
                 ])
          pure (jsonResponse ud)

-- | @POST /Users/:userId/PlayedItems/:itemId@ with @{\"Played\": true|false}@ —
-- mark played (bumping the play count and stamping @lastPlayedAt@) or
-- unplayed. 200 with the updated record; 400 on a malformed body.
playedHandler :: [(Text, Text)] -> Request -> AppM Response
playedHandler captures req = do
  (uid, iid) <- captureUserItem captures
  _ <- requireUserDataAccess req uid
  _ <- requireExistingItem iid
  body <- liftIO (strictRequestBody req)
  case eitherDecode body of
    Left err -> throwError (BadRequest ("invalid JSON body: " <> T.pack err))
    Right pr -> do
      now <- liftIO getCurrentTime
      let b = playedReqPlayed pr
      ud <- updateUserData uid iid $ \u ->
        let u' = markPlayed b u
        in if b then u' { udLastPlayedAt = Just now } else u'
      pure (jsonResponse ud)

-- | @DELETE /Users/:userId/PlayedItems/:itemId@ — mark unplayed.
unplayedHandler :: [(Text, Text)] -> Request -> AppM Response
unplayedHandler captures req = do
  (uid, iid) <- captureUserItem captures
  _ <- requireUserDataAccess req uid
  _ <- requireExistingItem iid
  updateUserData uid iid (markPlayed False) >>= pure . jsonResponse

-- | @POST /Users/:userId/FavoriteItems/:itemId@ — mark favorite.
favoriteHandler :: [(Text, Text)] -> Request -> AppM Response
favoriteHandler captures req = do
  (uid, iid) <- captureUserItem captures
  _ <- requireUserDataAccess req uid
  _ <- requireExistingItem iid
  updateUserData uid iid (setFavorite True) >>= pure . jsonResponse

-- | @DELETE /Users/:userId/FavoriteItems/:itemId@ — clear favorite.
unfavoriteHandler :: [(Text, Text)] -> Request -> AppM Response
unfavoriteHandler captures req = do
  (uid, iid) <- captureUserItem captures
  _ <- requireUserDataAccess req uid
  _ <- requireExistingItem iid
  updateUserData uid iid (setFavorite False) >>= pure . jsonResponse

-- | @GET /Users/:userId/Items@ (milestone 5f, docs/todo.org section 9) —
-- the watch-history view: the same envelope as @/items?userId=:userId@
-- (delegates to 'itemsQueryResponse' with @userId@ forced into the query
-- params), so @sortBy=LastPlayedDate/PlayCount@, @filters=...@ and
-- @fields=...@ all work. Self-or-admin guard on @:userId@.
-- | @GET \/Users/:userId/Views@ (milestone 17c) — 200
-- @{\"Items\": [...], \"TotalRecordCount\": n}@ with one CollectionFolder
-- view per media folder visible to the target user (non-admin: filtered by
-- 'allowedFoldersFor', same rule as 'visibleItems'). Self-or-admin on
-- :userId (403 cross-user), 404 unknown user.
viewsHandler :: [(Text, Text)] -> Request -> AppM Response
viewsHandler captures req = do
  uid <- capture "userId" captures
  u <- requireSelfOrAdmin403' req uid
  _ <- requireExistingUserOr404 uid
  folders <- listFolders
  let visible = [ f | f <- folders
                    , canAccessFolder (allowedFoldersFor u) (folderId f) ]
      views = viewsFromFolders visible
  pure (jsonResponse (object
    [ "Items" .= views
    , "TotalRecordCount" .= length views
    ]))

-- | @GET \/Users/:userId/Items/Latest?Limit=&IncludeItemTypes=@
-- (milestone 17c) — 200 @{\"Items\": [...], \"TotalRecordCount\": n}@ with
-- the newest items (DateCreated desc) visible to the target user, capped
-- at Limit (default 12). Self-or-admin on :userId, 404 unknown user.
latestItemsHandler :: [(Text, Text)] -> Request -> AppM Response
latestItemsHandler captures req = do
  uid <- capture "userId" captures
  u <- requireSelfOrAdmin403' req uid
  _ <- requireExistingUserOr404 uid
  items <- listItems >>= visibleItems (Just u)
  let qparams = queryParams (queryString req)
      limit = fromMaybe (12 :: Int) (lookup "limit" qparams >>= readMaybe . T.unpack)
      latest = latestItems qparams limit items
  pure (jsonResponse (object
    [ "Items" .= latest
    , "TotalRecordCount" .= length latest
    ]))

-- | @GET \/DisplayPreferences?userId=&client=@ (milestone 17c) — 200 the
-- stored prefs object for that user/client, or @{}@ when nothing is
-- stored. Self-or-admin on the userId param (400 when missing).
displayPrefsGetHandler :: [(Text, Text)] -> Request -> AppM Response
displayPrefsGetHandler _captures req = do
  let qparams = queryParams (queryString req)
  case lookup "userid" qparams of  -- query names are case-folded (17a)
    Nothing -> throwError (BadRequest "missing userId query param")
    Just uid -> do
      _ <- requireUserDataAccess req uid
      _ <- requireExistingUserOr404 uid
      let client = fromMaybe "" (lookup "client" qparams)
      mV <- getDisplayPrefs uid client
      pure (jsonResponse (fromMaybe (object []) mV))

-- | @POST \/DisplayPreferences?userId=&client=@ (milestone 17c) — store
-- the request body (any JSON value) as the prefs for that user/client,
-- persist to displayPreferences.json, 200 with the stored body.
-- Self-or-admin on userId, 400 on missing userId or bad JSON.
displayPrefsPostHandler :: [(Text, Text)] -> Request -> AppM Response
displayPrefsPostHandler _captures req = do
  let qparams = queryParams (queryString req)
  case lookup "userid" qparams of  -- query names are case-folded (17a)
    Nothing -> throwError (BadRequest "missing userId query param")
    Just uid -> do
      _ <- requireUserDataAccess req uid
      _ <- requireExistingUserOr404 uid
      let client = fromMaybe "" (lookup "client" qparams)
      body <- liftIO (strictRequestBody req)
      case eitherDecode body of
        Left err -> throwError (BadRequest ("invalid JSON body: " <> T.pack err))
        Right v  -> do
          setDisplayPrefs uid client v
          pure (jsonResponse v)

usersItemsHandler :: [(Text, Text)] -> Request -> AppM Response
usersItemsHandler captures req = do
  uid <- capture "userId" captures
  _ <- requireExistingUserOr404 uid
  -- self-or-admin, but 403 (not 401) for an authenticated non-admin
  -- hitting another users id (milestone 9 convention)
  u <- case parseTokenHeader [(original k, v) | (k, v) <- requestHeaders req] of
    Nothing -> throwError (Unauthorized "authentication required")
    Just tok -> do
      mUser <- resolveToken tok
      case mUser of
        Nothing -> throwError (Unauthorized "invalid or expired token")
        Just usr
          | isAdmin usr || userId usr == uid -> pure usr
          | otherwise -> throwError (Forbidden "self or admin required")
  itemsQueryResponse (Just u) (("userid", uid) : queryParams (queryString req))

-- | @DELETE /Users/:userId/PlayedItems@ with @{\"ItemIds\":[...]}@ (or
-- @{\"Ids\":[...]}@) — batch mark unplayed (milestone 5f). 200
-- @{\"status\":\"ok\",\"cleared\":n}@ where n is how many of the ids were
-- actually marked played before; 400 bad JSON, 404 unknown user.
batchUnplayedHandler :: [(Text, Text)] -> Request -> AppM Response
batchUnplayedHandler captures req = do
  uid <- capture "userId" captures
  _ <- requireUserDataAccess req uid
  _ <- requireExistingUserOr404 uid
  ids <- parseIdsBodyReq req
  n <- batchClear uid ids (markPlayed False) udPlayed
  pure (jsonResponse (object ["status" .= ("ok" :: Text), "cleared" .= n]))

-- | @DELETE /Users/:userId/FavoriteItems@ with @{\"ItemIds\":[...]}@ —
-- batch clear favorites (milestone 5f); same response shape as the played
-- batch delete.
batchUnfavoriteHandler :: [(Text, Text)] -> Request -> AppM Response
batchUnfavoriteHandler captures req = do
  uid <- capture "userId" captures
  _ <- requireUserDataAccess req uid
  _ <- requireExistingUserOr404 uid
  ids <- parseIdsBodyReq req
  n <- batchClear uid ids (setFavorite False) udFavorite
  pure (jsonResponse (object ["status" .= ("ok" :: Text), "cleared" .= n]))

-- Helpers -------------------------------------------------------------------

-- | Apply a setter to each of the ids and return how many of them matched
-- the predicate before the update (i.e. how many were actually cleared).
batchClear
  :: Text                       -- ^ userId
  -> [Text]                     -- ^ item ids
  -> (UserData -> UserData)      -- ^ setter (markPlayed False / setFavorite False)
  -> (UserData -> Bool)          -- ^ matcher (udPlayed / udFavorite)
  -> AppM Int
batchClear uid ids setter match = do
  ud <- listUserData uid
  let cleared = length [ i | i <- ids, maybe False match (HM.lookup i ud) ]
  mapM_ (\i -> updateUserData uid i setter) ids
  pure cleared

-- | Read + parse the request body as an id list; 400 on malformed JSON.
parseIdsBodyReq :: Request -> AppM [Text]
parseIdsBodyReq req = do
  body <- liftIO (strictRequestBody req)
  case parseIdsBody body of
    Left err -> throwError (BadRequest ("invalid JSON body: " <> T.pack err))
    Right ids -> pure ids

-- | The user must exist; 404 otherwise.
requireExistingUserOr404 :: Text -> AppM User
requireExistingUserOr404 uid = do
  m <- getUser uid
  case m of
    Nothing -> throwError (NotFound ("no user with id " <> uid))
    Just u  -> pure u


-- | Merge a partial JSON patch (a decoded object) into an existing record:
-- only the keys actually present in the patch override the stored values.
-- A present-but-null optional field clears it. Wrongly typed values are a
-- 'Left' (the caller turns that into a 400).
mergeUserDataPatch :: HashMap Text Value -> UserData -> Either String UserData
mergeUserDataPatch patch ud = do
  played     <- field "played"           (udPlayed ud)
  favorite   <- field "favorite"         (udFavorite ud)
  position   <- field "playbackPosition" (udPlaybackPosition ud)
  playCount  <- field "playCount"        (udPlayCount ud)
  lastPlayed <- field "lastPlayedAt"     (udLastPlayedAt ud)
  pure (UserData played favorite position playCount lastPlayed)
  where
    field :: FromJSON a => Text -> a -> Either String a
    field k def = case HM.lookup k patch of
      Nothing -> Right def
      Just v  -> parseEither parseJSON v

-- | The @(userId, itemId)@ path captures, or 400 when missing.
captureUserItem :: [(Text, Text)] -> AppM (Text, Text)
captureUserItem captures = do
  uid <- capture "userId" captures
  iid <- capture "itemId" captures
  pure (uid, iid)

capture :: Text -> [(Text, Text)] -> AppM Text
capture name captures =
  case lookup name captures of
    Nothing -> throwError (BadRequest ("missing " <> name <> " capture"))
    Just v  -> pure v

-- | Auth gate for these routes: the request must carry a valid token
-- ('parseTokenHeader' over the raw request headers, resolved via
-- 'MonadAuth'), and the path's @:userId@ must be the token user's own id or
-- the token user must be an admin. Anything else -> 401.
-- | Like 'requireUserDataAccess' but 403 (not 401) for an authenticated
-- non-admin hitting another user's id - the milestone-9/17c convention
-- used by /Users/:id/Items, /Views, /Items/Latest.
requireSelfOrAdmin403' :: Request -> Text -> AppM User
requireSelfOrAdmin403' req targetUserId =
  case parseTokenHeader [(original k, v) | (k, v) <- requestHeaders req] of
    Nothing -> throwError (Unauthorized "authentication required")
    Just tok -> do
      mUser <- resolveToken tok
      case mUser of
        Nothing -> throwError (Unauthorized "invalid or expired token")
        Just u
          | isAdmin u || userId u == targetUserId -> pure u
          | otherwise -> throwError (Forbidden "self or admin required")

requireUserDataAccess :: Request -> Text -> AppM User
requireUserDataAccess req targetUserId =
  case parseTokenHeader [(original k, v) | (k, v) <- requestHeaders req] of
    Nothing -> throwError (Unauthorized "authentication required")
    Just tok -> do
      mUser <- resolveToken tok
      case mUser of
        Nothing -> throwError (Unauthorized "invalid or expired token")
        Just u
          | isAdmin u || userId u == targetUserId -> pure u
          | otherwise -> throwError (Unauthorized "self or admin required")

-- | The item must exist in the library; 404 otherwise.
requireExistingItem :: Text -> AppM MediaItem
requireExistingItem iid = do
  m <- getItem iid
  case m of
    Nothing -> throwError (NotFound ("no library item with id " <> iid))
    Just item -> pure item
