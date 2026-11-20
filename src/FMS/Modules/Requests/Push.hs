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
  , parseTorrentStatuses
  , delugeTorrentStatuses
  , matchTorrentProgress
  , importComplete
  , getFirstHostId
  , delugeAddMagnetBody
  , delugeAddFileBody
  , pushRequest
  , delugeLoginTest
  , parseProxiedPath
  ) where

import Control.Exception (SomeException, try)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Char (isDigit)
import Data.Maybe (listToMaybe)
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
  , RequestBody (..)
  )
import Network.HTTP.Client (responseTimeoutMicro)
import Network.HTTP.Conduit (httpLbs, newManager, parseRequest_)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.CaseInsensitive as CI
import Network.HTTP.Types (hContentType, hCookie, statusCode)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), (<.>), takeExtension, takeFileName)

import FMS.Modules.Requests.Types (normalizeTitle)

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
matchesDownload title files =
  let target = normalizeTitle title
      stripExt p =
        let e = takeExtension p
        in if null e then p else take (length p - length e) p
      matches f =
        let nf = normalizeTitle (T.pack (stripExt (takeFileName f)))
            yearSuffix r = not (T.null target) && T.length (T.strip r) == 4 && T.all isDigit (T.strip r)
        in nf == target || maybe False yearSuffix (T.stripPrefix target nf)
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
              case mMagnet of
                Just mag -> do
                  er <- postJson manager delugeUrl cookie (delugeAddMagnetBody mag downloadDir trackers 4)
                  pure (rpcOk er)
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
  listToMaybe [ (prog, st) | (name, st, prog, _rate) <- torrents, matchesOne name ]
  where
    nt = normalizeTitle title
    cands = nt : maybe [] (\rel -> [normalizeTitle rel]) mRelease
    matchesOne name =
      let nn = normalizeTitle name
      in any (\c -> not (T.null c) && (nn == c || T.isInfixOf c nn || T.isInfixOf nn c)) cands

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
