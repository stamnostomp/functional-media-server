module FMS.Modules.Requests.Push
  ( slugify
  , autoApproveSize
  , matchesDownload
  , matchesRequestDownload
  , base64Encode
  , delugeLoginBody
  , delugeGetHostsBody
  , delugeConnectBody
  , delugeConnectedBody
  , delugeTorrentsStatusBody
  , delugeSetConfigBody
  , parseTorrentStatuses
  , delugeTorrentStatuses
  , matchTorrentProgress
  , matchTorrentName
  , matchesTorrentTitle
  , matchesDownloadAny
  , importComplete
  , delugeTorrentsStatusRichBody
  , parseTorrentStatusesRich
  , delugeTorrentStatusesRich
  , btihFromMagnet
  , delugeReaddTorrent
  , getFirstHostId
  , delugeAddMagnetBody
  , delugeSetTrackersBody
  , delugeAddFileBody
  , pushRequest
  , delugeLoginTest
  , parseProxiedPath
  ) where

import Control.Exception (SomeException, try)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Char (isDigit)
import Data.Maybe (catMaybes, listToMaybe)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson (Value, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector as V
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Network.HTTP.Client
  ( Manager
  , managerResponseTimeout
  , requestBody
  , requestHeaders
  , method
  , responseBody
  , responseHeaders
  , responseStatus
  , responseTimeout
  , ResponseTimeout (..)
  , RequestBody (..)
  )
import Network.HTTP.Client (ResponseTimeout (..), responseTimeoutMicro)
import Network.HTTP.Conduit (httpLbs, newManager, parseRequest_)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.CaseInsensitive as CI
import Network.HTTP.Types (hContentType, hCookie, statusCode)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), (<.>), takeExtension, takeFileName)

import FMS.Modules.Metadata.Api.Types (cleanTitleForProvider)
import FMS.Modules.Requests.Types (normalizeTitle)
import FMS.Modules.Library.Scan (isMediaFile)

-- | Pure: slug for fallback file names -- normalized title with spaces as
-- dashes; empty becomes "request".
slugify :: Text -> String
slugify t =
  let s = map (\c -> if c == ' ' then '-' else c) (T.unpack (normalizeTitle t))
  in if null s then "request" else s

-- | Pure: auto-approve decision (m20b). No known size -> approve; a known
-- size at or under @thresholdBytes@ -> approve.
autoApproveSize :: Maybe Integer -> Integer -> Bool
autoApproveSize mSize thresholdBytes =
  maybe True (<= thresholdBytes) mSize

-- | Pure: does any file name in the download dir normalize to the same
-- title as the request (extension stripped first)? Used to mark requests
-- Imported when a matching media file appears in the download dir.
matchesDownload :: Text -> [FilePath] -> Bool
matchesDownload title files0 =
  -- m97: only real media files can satisfy a request -- the .magnet/.nfo
  -- sidecars fms writes into the download dir normalize to the bare title
  -- and used to false-match (wrong-version import)
  let files = filter isMediaFile files0
      target = normalizeTitle title
      stripExt p =
        let e = takeExtension p
        in if null e then p else take (length p - length e) p
      -- m80: match against EVERY path component (Deluge saves into
      -- <downloads>/<Torrent.Name>/<release file>), not just the file name
      comps f = filter (not . T.null) (T.splitOn "/" (T.pack f))
      matches f =
        let nf c = normalizeTitle (T.pack (stripExt (T.unpack c)))
            n0 c = normalizeTitle c
            yearSuffix r = not (T.null target) && T.length (T.strip r) == 4 && T.all isDigit (T.strip r)
            -- m80b: compare the raw component too -- takeExtension mangles
            -- folder names containing dots in brackets (e.g. "[YTS.AM]")
            compOk c = let n = nf c
                       in n == target || n0 c == target || maybe False yearSuffix (T.stripPrefix target n)
        in any compOk (comps f)
  in any matches files

-- | m31: like 'matchesDownload' but also tries the SOURCE RELEASE name
-- (Deluge names files after the release, not the request title -- e.g.
-- title \"Shrek 2001\" vs file \"Shrek.2001.1080p.BluRay.x265-LAMA.mkv\").
-- Pure: title match OR release match.
matchesRequestDownload :: Text -> Maybe Text -> [FilePath] -> Bool
matchesRequestDownload title mRelease files =
  matchesDownload title files
  || maybe False (\rel -> matchesDownload rel files) mRelease

-- | Pure base64 encoder (RFC 4648 with padding; the @base64@ package is
-- not a dependency, so this is implemented here with bytestring only).
base64Encode :: BS.ByteString -> Text
base64Encode bs = T.pack (go (BSC.unpack bs))
  where
    alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/" :: String
    go []           = ""
    go (a : b : c : rest) =
      let n = (fromEnum a `shiftL` 16) .|. (fromEnum b `shiftL` 8) .|. fromEnum c :: Int
      in [ alphabet !! ((n `shiftR` 18) .&. 63), alphabet !! ((n `shiftR` 12) .&. 63)
         , alphabet !! ((n `shiftR` 6) .&. 63), alphabet !! (n .&. 63) ] ++ go rest
    go [a, b] =
      let n = (fromEnum a `shiftL` 16) .|. (fromEnum b `shiftL` 8) :: Int
      in [ alphabet !! ((n `shiftR` 18) .&. 63), alphabet !! ((n `shiftR` 12) .&. 63)
         , alphabet !! ((n `shiftR` 6) .&. 63), '=' ]
    go [a] =
      let n = fromEnum a `shiftL` 16 :: Int
      in [ alphabet !! ((n `shiftR` 18) .&. 63), alphabet !! ((n `shiftR` 12) .&. 63), '=', '=' ]

-- | Pure: Deluge web JSON-RPC login body.
delugeLoginBody :: Text -> Int -> Value
delugeLoginBody password rid =
  object [ "method" .= ("auth.login" :: Text), "params" .= [Aeson.String password], "id" .= rid ]

-- | Pure: Deluge web.get_hosts body (m29: needed to find the daemon host id
-- for web.connect -- after auth.login a deluge-web session is NOT yet
-- connected to the daemon).
delugeGetHostsBody :: Int -> Value
delugeGetHostsBody rid =
  object [ "method" .= ("web.get_hosts" :: Text), "params" .= ([] :: [Text]), "id" .= rid ]

-- | Pure: Deluge web.connect body -- connect the web session to the daemon
-- host with the given id (m29).
delugeConnectBody :: Text -> Int -> Value
delugeConnectBody hostId rid =
  object [ "method" .= ("web.connect" :: Text), "params" .= [Aeson.String hostId], "id" .= rid ]

-- | Pure: Deluge web.connected body -- is the web session connected to a
-- daemon? (m29, used by the settings Test endpoint).
delugeConnectedBody :: Int -> Value
delugeConnectedBody rid =
  object [ "method" .= ("web.connected" :: Text), "params" .= ([] :: [Text]), "id" .= rid ]

-- | Pure: extract the FIRST host id from a web.get_hosts JSON-RPC response
-- (result is an array of [id, host, port, user, password] arrays). The id
-- is a string in deluge-web.
getFirstHostId :: LBS.ByteString -> Maybe String
getFirstHostId b = do
  v <- Aeson.decode b
  case v of
    Aeson.Object o -> case KeyMap.lookup (Key.fromText "result") o of
      Just (Aeson.Array arr) -> case V.toList arr of
        (Aeson.Array host : _) -> case V.toList host of
          (Aeson.String hid : _) -> Just (T.unpack hid)
          _ -> Nothing
        _ -> Nothing
      _ -> Nothing
    _ -> Nothing

-- | Pure: the Deluge per-add options object: download_location plus
-- @add_trackers@ (m62: the refreshed public announce-tracker list) when
-- non-empty.
delugeAddOptions :: FilePath -> [Text] -> Value
delugeAddOptions downloadDir trackers =
  object ([ "download_location" .= downloadDir ]
          ++ [ "add_trackers" .= trackers | not (null trackers) ])

-- | Pure: Deluge core.add_torrent_magnet body.
delugeAddMagnetBody :: Text -> FilePath -> [Text] -> Int -> Value
delugeAddMagnetBody magnet downloadDir trackers rid =
  object [ "method" .= ("core.add_torrent_magnet" :: Text)
         , "params" .= [ Aeson.String magnet
                       , delugeAddOptions downloadDir trackers ]
         , "id" .= rid ]

-- | Pure: Deluge core.add_torrent_file body (base64 filedump).
delugeAddFileBody :: Text -> Text -> FilePath -> [Text] -> Int -> Value
delugeAddFileBody name b64 downloadDir trackers rid =
  object [ "method" .= ("core.add_torrent_file" :: Text)
         , "params" .= [ Aeson.String name, Aeson.String b64
                       , delugeAddOptions downloadDir trackers ]
         , "id" .= rid ]

-- | m79: Deluge core.set_torrent_trackers body — attaches the announce
-- tracker list to an EXISTING torrent as [{"url":..,"tier":0}] entries
-- (the add-time @add_trackers@ option is silently ignored by some
-- deluge-web/daemon builds, which left pushed magnets with zero trackers).
delugeSetTrackersBody :: Text -> [Text] -> Int -> Value
delugeSetTrackersBody torrentId trackers rid =
  object [ "method" .= ("core.set_torrent_trackers" :: Text)
         , "params" .= [ Aeson.String torrentId
                       , Aeson.toJSON [ object [ "url" .= u, "tier" .= (0 :: Int) ] | u <- trackers ] ]
         , "id" .= rid ]

-- | Extract the "result" field of a JSON-RPC response body.
rpcResult :: LBS.ByteString -> Maybe Value
rpcResult b = Aeson.decode b >>= \v -> case v of
  Aeson.Object o -> KeyMap.lookup (Key.fromText "result") o
  _ -> Nothing

-- | True when a Deluge JSON-RPC "result" is a success (non-null, not
-- @false@).
rpcResultOk :: Maybe Value -> Bool
rpcResultOk r = case r of
  Just (Aeson.String _) -> True
  Just (Aeson.Bool False) -> False
  Just Aeson.Null -> False
  Just _  -> True
  Nothing -> False

-- | POST a JSON body (optionally with a Cookie header); returns the
-- response body and the session cookie from Set-Cookie.
postJson :: Manager -> Text -> Maybe Text -> Value -> IO (Either String (LBS.ByteString, Maybe Text))
postJson manager url mCookie body = do
  eres <- try $ do
    let req0 = parseRequest_ (T.unpack url)
        req1 = req0 { method = "POST"
                    , requestHeaders = [(hContentType, "application/json")]
                    , requestBody = RequestBodyLBS (Aeson.encode body)
                    , responseTimeout = responseTimeoutMicro 5000000  -- m89: never stall /Requests on a hung deluge
                    }
        req2 = case mCookie of
          Just c  -> req1 { requestHeaders = (hCookie, TE.encodeUtf8 c) : requestHeaders req1 }
          Nothing -> req1
    resp <- httpLbs req2 manager
    if statusCode (responseStatus resp) == 200
      then pure (responseBody resp, TE.decodeUtf8 <$> lookup (CI.mk "Set-Cookie") (responseHeaders resp))
      else error ("http " ++ show (statusCode (responseStatus resp)))
  case eres of
    Left (e :: SomeException) -> pure (Left ("http failed: " ++ show e))
    Right r                   -> pure (Right r)

-- | GET raw bytes with a 10s timeout.
fetchBytes :: Text -> IO (Either String BS.ByteString)
fetchBytes url = do
  manager <- newManager (tlsManagerSettings
    { managerResponseTimeout = responseTimeoutMicro 10000000 })
  eres <- try (httpLbs (parseRequest_ (T.unpack url)) manager)
  case eres of
    Left (e :: SomeException) -> pure (Left ("fetch failed: " ++ show e))
    Right resp
      | statusCode (responseStatus resp) == 200 -> pure (Right (LBS.toStrict (responseBody resp)))
      | otherwise -> pure (Left ("fetch returned HTTP " ++ show (statusCode (responseStatus resp))))

-- | m29: test a Deluge connection the way the push path uses it:
-- @auth.login@, then @web.get_hosts@ + @web.connect@ with the first host id
-- (deluge-web sessions are NOT connected to the daemon after login alone),
-- then @web.connected@ must report @true@. Used by
-- @POST /Requests/Settings/Test@. Never throws; Left carries the error.
delugeLoginTest :: Text -> Text -> IO (Either String ())
delugeLoginTest delugeUrl delugePassword = do
  eres <- try $ do
    manager <- newManager (tlsManagerSettings
      { managerResponseTimeout = responseTimeoutMicro 10000000 })
    eLogin <- postJson manager delugeUrl Nothing (delugeLoginBody delugePassword 1)
    case eLogin of
      Left err -> pure (Left err)
      Right (body, cookie) ->
        case rpcResult body of
          Just res | rpcResultOk (Just res) -> do
            eHosts <- postJson manager delugeUrl cookie (delugeGetHostsBody 2)
            case eHosts of
              Left err -> pure (Left err)
              Right (hbody, _) ->
                case getFirstHostId hbody of
                  Nothing -> pure (Left "deluge web.get_hosts returned no hosts")
                  Just hid -> do
                    _ <- postJson manager delugeUrl cookie (delugeConnectBody (T.pack hid) 3)
                    eConn <- postJson manager delugeUrl cookie (delugeConnectedBody 4)
                    case eConn of
                      Left err -> pure (Left err)
                      Right (cbody, _) ->
                        case rpcResult cbody of
                          Just (Aeson.Bool True) -> pure (Right ())
                          _ -> pure (Left "deluge web.connected is false after web.connect")
          _ -> pure (Left "deluge auth.login failed")
  case eres of
    Left (e :: SomeException) -> pure (Left ("http failed: " ++ show e))
    Right r                   -> pure r

-- | Parse a same-origin proxied indexer download path
-- @/Indexers/<id>/Download?r=<releaseId>@ into @(indexerId, releaseId)@.
-- Pure; used so the push pipeline can resolve proxied URLs in-process
-- instead of HTTP-fetching its own auth-gated route (m21 fix).
parseProxiedPath :: Text -> Maybe (Text, Text)
parseProxiedPath u =
  case T.stripPrefix "/Indexers/" u of
    Nothing -> Nothing
    Just rest ->
      let (iid, rq) = T.breakOn "/Download" rest
      in case T.stripPrefix "?r=" (T.drop 9 rq) of
           Just r | not (T.null iid) && not (T.null r) -> Just (iid, r)
           _ -> Nothing

-- | Push a release to Deluge (magnet preferred, else download the
-- .torrent and add_torrent_file). On ANY failure, fall back to writing the
-- magnet / torrent bytes into the download dir so an external client or
-- the auto-import can pick it up. Returns Right () when the release is
-- either pushed or written to the download dir, Left err otherwise.
-- The @resolveProxied@ callback resolves same-origin @/Indexers/...@
-- download URLs in-process (see 'parseProxiedPath').
pushRequest
  :: Text       -- ^ deluge JSON-RPC url
  -> Text       -- ^ deluge password
  -> FilePath   -- ^ download dir
  -> Maybe Text -- ^ magnet
  -> Maybe Text -- ^ download (torrent) url
  -> Text       -- ^ title (for the fallback slug)
  -> [Text]     -- ^ announce trackers to attach (m62: add_trackers option)
  -> (Text -> IO (Either String BS.ByteString)) -- ^ in-process resolver for proxied /Indexers/ URLs
  -> IO (Either String ())
pushRequest delugeUrl delugePassword downloadDir mMagnet mDownloadUrl title trackers resolveProxied = do
  createDirectoryIfMissing True downloadDir
  case (mMagnet, mDownloadUrl) of
    (Nothing, Nothing) -> pure (Left "no magnet or download url on request")
    _ -> do
      pushed <- tryDeluge
      case pushed of
        Right () -> pure (Right ())
        Left _   -> writeFallback
  where
    slug = slugify title
    -- proxied /Indexers/... URLs are resolved in-process; anything else is
    -- fetched over HTTP directly
    fetchSource u
      | Just _ <- parseProxiedPath u = resolveProxied u
      | otherwise = fetchBytes u
    newMgr = newManager (tlsManagerSettings
      { managerResponseTimeout = responseTimeoutMicro 10000000 })
    tryDeluge = do
      manager <- newMgr
      eLogin <- postJson manager delugeUrl Nothing (delugeLoginBody delugePassword 1)
      case eLogin of
        Left err -> pure (Left err)
        Right (body, cookie) ->
          case rpcResult body of
            Just res | rpcResultOk (Just res) -> do
              -- m29: after auth.login a deluge-web session is NOT connected
              -- to the daemon (web.connected = false), so core.* would
              -- answer "Unknown method". Connect to the first web.get_hosts
              -- host first; if that fails, still attempt the add as before.
              _ <- tryConnect manager cookie  -- best-effort: ignore the result
              -- m80: re-assert uTP + unlimited queue before adding
              -- (best-effort; id 6 is free after login/hosts/connect/add)
              _ <- postJson manager delugeUrl cookie (delugeSetConfigBody 6)
              case mMagnet of
                Just mag -> do
                  er <- postJson manager delugeUrl cookie (delugeAddMagnetBody mag downloadDir trackers 4)
                  -- m79: add_torrent_magnet returns the info hash; some
                  -- deluge builds ignore the add_trackers option, so attach
                  -- the announce list explicitly afterwards (best-effort)
                  case er of
                    Right (body, _)
                      | rpcResultOk (rpcResult body)
                      , not (null trackers)
                      , Just (Aeson.String tid) <- rpcResult body -> do
                          _ <- postJson manager delugeUrl cookie (delugeSetTrackersBody tid trackers 5)
                          pure (Right ())
                    _ -> pure (rpcOk er)
                Nothing -> case mDownloadUrl of
                  Just u -> do
                    ef <- fetchSource u
                    case ef of
                      Left err -> pure (Left err)
                      Right bytes -> do
                        er <- postJson manager delugeUrl cookie
                          (delugeAddFileBody (T.pack (slug <.> "torrent")) (base64Encode bytes) downloadDir trackers 4)
                        pure (rpcOk er)
                  Nothing -> pure (Left "no magnet")
            _ -> pure (Left "deluge auth.login failed")
    -- web.get_hosts (id 2) -> web.connect (id 3) with the first host id;
    -- returns True when a connect call succeeded, False otherwise. Never
    -- throws.
    tryConnect manager cookie = do
      eHosts <- postJson manager delugeUrl cookie (delugeGetHostsBody 2)
      case eHosts of
        Left _ -> pure False
        Right (hbody, _) ->
          case getFirstHostId hbody of
            Nothing -> pure False
            Just hid -> do
              eConn <- postJson manager delugeUrl cookie (delugeConnectBody (T.pack hid) 3)
              case eConn of
                Left _ -> pure False
                Right (cbody, _) -> pure (rpcResultOk (rpcResult cbody))
    rpcOk (Left err) = Left err
    rpcOk (Right (body, _)) =
      if rpcResultOk (rpcResult body)
        then Right ()
        else Left "deluge rpc returned no result"
    writeFallback =
      case mMagnet of
        Just mag -> do
          BS.writeFile (downloadDir </> (slug <.> "magnet")) (TE.encodeUtf8 mag)
          pure (Right ())
        Nothing -> case mDownloadUrl of
          Just u -> do
            ef <- fetchSource u
            case ef of
              Left err -> pure (Left err)
              Right bytes -> do
                BS.writeFile (downloadDir </> (slug <.> "torrent")) bytes
                pure (Right ())
          Nothing -> pure (Left "no download source")

-- | m80: core.set_config body that re-asserts the client settings FMS needs
-- on every push/poll (best-effort): uTP both ways with peer-proportional
-- mixed mode (the NAT here blocks outbound TCP to classic peer ports, so
-- uTP/UDP is what actually carries peer traffic), and unlimited active
-- queues (a stuck 0% torrent must not wedge the queue and starve new
-- requests). Values are libtorrent settings_pack keys that Deluge passes
-- straight through to the session.
delugeSetConfigBody :: Int -> Value
delugeSetConfigBody rid =
  object [ "method" .= ("core.set_config" :: Text)
         , "params" .= [ object
             [ "enable_outgoing_utp" .= True
             , "enable_incoming_utp" .= True
             , "mixed_mode_algorithm" .= (1 :: Int)
             , "max_active_downloading" .= (-1 :: Int)
             , "max_active_seeding" .= (-1 :: Int)
             , "max_active_limit" .= (-1 :: Int)
             , "max_downloads_global" .= (-1 :: Int)
             , "dont_count_slow_torrents" .= False
             ] ]
         , "id" .= rid ]

-- | Pure: Deluge core.get_torrents_status body (m37) -- all torrents, asking
-- only for the keys the progress bar needs.
delugeTorrentsStatusBody :: Int -> Value
delugeTorrentsStatusBody rid =
  object [ "method" .= ("core.get_torrents_status" :: Text)
         , "params" .= [ object []
                       , Aeson.toJSON (["name", "state", "progress", "download_payload_rate"] :: [Text]) ]
         , "id" .= rid ]

-- | Pure: parse a core.get_torrents_status result object (torrent id ->
-- status object) into @(name, state, progress 0-100, download_payload_rate)@.
-- Torrents missing any key are skipped.
parseTorrentStatuses :: Aeson.Value -> [(Text, Text, Double, Double)]
parseTorrentStatuses (Aeson.Object o) =
  [ (name, state, prog, rate)
  | Aeson.Object to <- KeyMap.elems o
  , Just (Aeson.String name)  <- [KeyMap.lookup (Key.fromText "name") to]
  , Just (Aeson.String state) <- [KeyMap.lookup (Key.fromText "state") to]
  , Just prog <- [numField "progress" to]
  , let rate = fromMaybe0 (numField "download_payload_rate" to)
  ]
  where
    numField k m = case KeyMap.lookup (Key.fromText k) m of
      Just (Aeson.Number n) -> Just (realToFrac n)
      _ -> Nothing
    fromMaybe0 (Just x) = x
    fromMaybe0 Nothing  = 0
parseTorrentStatuses _ = []

-- | m37: query the Deluge web API for live torrent statuses. Same
-- never-throw pattern as 'pushRequest': auth.login (id 1), best-effort
-- web.get_hosts (id 2) + web.connect (id 3), then core.get_torrents_status
-- (id 4). Returns @(name, state, progress, download_payload_rate)@ per
-- torrent; Left err on any failure.
delugeTorrentStatuses :: Text -> Text -> IO (Either String [(Text, Text, Double, Double)])
delugeTorrentStatuses delugeUrl delugePassword = do
  eres <- try $ do
    manager <- newManager (tlsManagerSettings
      { managerResponseTimeout = responseTimeoutMicro 10000000 })
    eLogin <- postJson manager delugeUrl Nothing (delugeLoginBody delugePassword 1)
    case eLogin of
      Left err -> pure (Left err)
      Right (body, cookie) ->
        case rpcResult body of
          Just res | rpcResultOk (Just res) -> do
            eHosts <- postJson manager delugeUrl cookie (delugeGetHostsBody 2)
            case eHosts of
              Left _ -> pure ()   -- best-effort connect: proceed anyway
              Right (hbody, _) ->
                case getFirstHostId hbody of
                  Nothing -> pure ()
                  Just hid -> do
                    _ <- postJson manager delugeUrl cookie (delugeConnectBody (T.pack hid) 3)
                    -- m80: re-assert uTP + unlimited queue (best-effort)
                    _ <- postJson manager delugeUrl cookie (delugeSetConfigBody 5)
                    pure ()
            eSt <- postJson manager delugeUrl cookie (delugeTorrentsStatusBody 4)
            case eSt of
              Left err -> pure (Left err)
              Right (sbody, _) ->
                case rpcResult sbody of
                  Just v -> pure (Right (parseTorrentStatuses v))
                  Nothing -> pure (Left "deluge get_torrents_status returned no result")
          _ -> pure (Left "deluge auth.login failed")
  case eres of
    Left (e :: SomeException) -> pure (Left ("http failed: " ++ show e))
    Right r                   -> pure r

-- | m37: pure matcher -- find the live torrent matching a request by
-- normalized name equality OR containment against the request title /
-- source release name. Returns the first match's @(progress, state)@.
matchTorrentProgress
  :: Text                              -- ^ request title
  -> Maybe Text                        -- ^ source release name (optional)
  -> [(Text, Text, Double, Double)]    -- ^ (torrent name, state, progress, rate)
  -> Maybe (Double, Text)
matchTorrentProgress title mRelease torrents =
  case matchTorrentName' title mRelease torrents of
    Nothing -> Nothing
    Just (name, st, prog) -> Just (prog, st)

-- | m80: like 'matchTorrentProgress' but also returns the matched live
-- torrent's NAME -- Deluge names the download folder/files after the torrent
-- name, not the request title, so file matching needs it when the release
-- name differs (e.g. request "Shrek 2001" pushed as a magnet whose swarm
-- name is "Shrek (2001) [1080p]").
matchTorrentName
  :: Text
  -> Maybe Text
  -> [(Text, Text, Double, Double)]
  -> Maybe Text
matchTorrentName title mRelease torrents =
  case matchTorrentName' title mRelease torrents of
    Nothing -> Nothing
    Just (name, _st, _prog) -> Just name

matchTorrentName'
  :: Text -> Maybe Text -> [(Text, Text, Double, Double)] -> Maybe (Text, Text, Double)
matchTorrentName' title mRelease torrents =
  listToMaybe [ (name, st, prog) | (name, st, prog, _rate) <- torrents, matchesOne name ]
  where
    matchesOne = matchesTorrentTitle title mRelease

-- | Pure: compare a torrent name against a request title and optional
-- source release name. Titles are cleaned of scene tags and release years;
-- when both sides carry a four-digit year they must agree, so a torrent
-- for a different year's release cannot match a request. Containment is
-- retained for the tag-stripped titles (e.g. torrent "Other Movie 2020"
-- still matches the untagged request title "Other Movie").
matchesTorrentTitle :: Text -> Maybe Text -> Text -> Bool
matchesTorrentTitle title mRelease name =
  let (ct, yTitle) = cleanTitleForProvider title
      mRel = fmap cleanTitleForProvider mRelease
      years = catMaybes [yTitle, mRel >>= snd]
      (cn, yName) = cleanTitleForProvider name
      -- Enforce the year only when both sides carry one. A missing year
      -- supplies no evidence of a mismatch, while conflicting explicit
      -- years must not match.
      yearOk = maybe True (\y -> null years || y `elem` years) yName
      titleOk c = not (T.null c)
               && (c == cn || T.isInfixOf c cn || T.isInfixOf cn c)
  in yearOk && any titleOk (ct : maybe [] (pure . fst) mRel)

-- | m80: does any candidate name match a file in the download dir? Used to
-- also try the live Deluge torrent name (see 'matchTorrentName') when the
-- request title / release name do not match the downloaded file name.
matchesDownloadAny :: [Text] -> [FilePath] -> Bool
matchesDownloadAny cands files = any (\c -> matchesDownload c files) cands

-- | m82: like 'delugeTorrentsStatusBody' but also asks num_seeds/num_peers
-- (the requests janitor needs them to detect stalled torrents).
delugeTorrentsStatusRichBody :: Int -> Value
delugeTorrentsStatusRichBody rid =
  object [ "method" .= ("core.get_torrents_status" :: Text)
         , "params" .= [ object []
                       , Aeson.toJSON (["name", "state", "progress", "download_payload_rate", "num_seeds", "num_peers"] :: [Text]) ]
         , "id" .= rid ]

-- | m82: parse the rich status object into
-- @(name, state, progress 0-100, rate, num_seeds, num_peers)@.
parseTorrentStatusesRich :: Aeson.Value -> [(Text, Text, Double, Double, Int, Int)]
parseTorrentStatusesRich (Aeson.Object o) =
  [ (name, state, prog, rate, seeds, peers)
  | Aeson.Object to <- KeyMap.elems o
  , Just (Aeson.String name)  <- [KeyMap.lookup (Key.fromText "name") to]
  , Just (Aeson.String state) <- [KeyMap.lookup (Key.fromText "state") to]
  , Just prog <- [numField "progress" to]
  , let rate  = fromMaybe0 (numField "download_payload_rate" to)
        seeds = fromMaybeI0 (intField "num_seeds" to)
        peers = fromMaybeI0 (intField "num_peers" to)
  ]
  where
    numField k m = case KeyMap.lookup (Key.fromText k) m of
      Just (Aeson.Number n) -> Just (realToFrac n)
      _ -> Nothing
    intField k m = case KeyMap.lookup (Key.fromText k) m of
      Just (Aeson.Number n) -> Just (round n)
      _ -> Nothing
    fromMaybe0 (Just x) = x
    fromMaybe0 Nothing  = 0
    fromMaybeI0 (Just x) = x
    fromMaybeI0 Nothing  = 0
parseTorrentStatusesRich _ = []

-- | m82: like 'delugeTorrentStatuses' but returns the rich 6-tuple.
delugeTorrentStatusesRich :: Text -> Text -> IO (Either String [(Text, Text, Double, Double, Int, Int)])
delugeTorrentStatusesRich delugeUrl delugePassword = do
  eres <- try $ do
    manager <- newManager (tlsManagerSettings
      { managerResponseTimeout = responseTimeoutMicro 10000000 })
    eLogin <- postJson manager delugeUrl Nothing (delugeLoginBody delugePassword 1)
    case eLogin of
      Left err -> pure (Left err)
      Right (body, cookie) ->
        case rpcResult body of
          Just res | rpcResultOk (Just res) -> do
            eHosts <- postJson manager delugeUrl cookie (delugeGetHostsBody 2)
            case eHosts of
              Left _ -> pure ()
              Right (hbody, _) ->
                case getFirstHostId hbody of
                  Nothing -> pure ()
                  Just hid -> do
                    _ <- postJson manager delugeUrl cookie (delugeConnectBody (T.pack hid) 3)
                    _ <- postJson manager delugeUrl cookie (delugeSetConfigBody 5)
                    pure ()
            eSt <- postJson manager delugeUrl cookie (delugeTorrentsStatusRichBody 4)
            case eSt of
              Left err -> pure (Left err)
              Right (sbody, _) ->
                case rpcResult sbody of
                  Just v -> pure (Right (parseTorrentStatusesRich v))
                  Nothing -> pure (Left "deluge get_torrents_status returned no result")
          _ -> pure (Left "deluge auth.login failed")
  case eres of
    Left (e :: SomeException) -> pure (Left ("http failed: " ++ show e))
    Right r                   -> pure r

-- | m82: extract the info hash from @magnet:?xt=urn:btih:<hash>&...@.
btihFromMagnet :: Text -> Maybe Text
btihFromMagnet mag =
  case T.splitOn "urn:btih:" (T.toLower mag) of
    _ : rest : _ ->
      let h = T.take 40 rest
          hex c = (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')
      in if T.length h == 40 && T.all hex h then Just (T.toLower h) else Nothing
    _ -> Nothing

-- | m82: remove + re-add a stalled torrent (fresh peer state). Login,
-- connect, re-assert client config, remove (keep data), re-add the magnet,
-- attach the announce trackers, reannounce, resume. The janitor calls this
-- for Downloading requests whose live torrent has ~0 progress and 0 seeds.
delugeReaddTorrent
  :: Text       -- ^ deluge JSON-RPC url
  -> Text       -- ^ deluge password
  -> FilePath   -- ^ download dir
  -> Text       -- ^ magnet
  -> Text       -- ^ info hash (btih)
  -> [Text]     -- ^ announce trackers
  -> IO (Either String ())
delugeReaddTorrent delugeUrl delugePassword downloadDir magnet hash trackers = do
  eres <- try $ do
    manager <- newManager (tlsManagerSettings
      { managerResponseTimeout = responseTimeoutMicro 10000000 })
    eLogin <- postJson manager delugeUrl Nothing (delugeLoginBody delugePassword 1)
    case eLogin of
      Left err -> pure (Left err)
      Right (body, cookie) ->
        case rpcResult body of
          Just res | rpcResultOk (Just res) -> do
            eHosts <- postJson manager delugeUrl cookie (delugeGetHostsBody 2)
            case eHosts of
              Left _ -> pure ()
              Right (hbody, _) ->
                case getFirstHostId hbody of
                  Nothing -> pure ()
                  Just hid -> do
                    _ <- postJson manager delugeUrl cookie (delugeConnectBody (T.pack hid) 3)
                    _ <- postJson manager delugeUrl cookie (delugeSetConfigBody 4)
                    pure ()
            _ <- postJson manager delugeUrl cookie
                   (object [ "method" .= ("core.remove_torrent" :: Text)
                           , "params" .= [Aeson.toJSON hash, Aeson.toJSON False]
                           , "id" .= (5 :: Int) ])
            _ <- postJson manager delugeUrl cookie (delugeAddMagnetBody magnet downloadDir trackers 6)
            _ <- postJson manager delugeUrl cookie (delugeSetTrackersBody hash trackers 7)
            _ <- postJson manager delugeUrl cookie
                   (object [ "method" .= ("core.force_reannounce" :: Text)
                           , "params" .= [[hash]]
                           , "id" .= (8 :: Int) ])
            _ <- postJson manager delugeUrl cookie
                   (object [ "method" .= ("core.resume_torrent" :: Text)
                           , "params" .= [[hash]]
                           , "id" .= (9 :: Int) ])
            pure (Right ())
          _ -> pure (Left "deluge auth.login failed")
  case eres of
    Left (e :: SomeException) -> pure (Left ("http failed: " ++ show e))
    Right r                   -> pure r

-- | m38: should a Downloading request be marked Imported? When a live
-- Deluge torrent matches (by title/release name), only when it is complete
-- (progress >= 99.9 or state Seeding) -- Deluge creates the file
-- immediately and fills it progressively. When no torrent matches (fallback
-- file mode, Deluge down), keep the file-exists behavior.
importComplete :: [(Text, Text, Double, Double)] -> Text -> Maybe Text -> Bool
importComplete statuses title mRelease =
  case matchTorrentProgress title mRelease statuses of
    Just (p, st) -> p >= 99.9 || st == "Seeding"
    Nothing      -> True
