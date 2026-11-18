module FMS.Modules.Requests.Push
  ( slugify
  , autoApproveSize
  , matchesDownload
  , base64Encode
  , delugeLoginBody
  , delugeAddMagnetBody
  , delugeAddFileBody
  , pushRequest
  ) where

import Control.Exception (SomeException, try)
import Data.Bits ((.&.), (.|.), shiftL, shiftR)
import Data.Char (isDigit)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import Data.Aeson (Value, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
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

-- | Pure: Deluge core.add_torrent_magnet body.
delugeAddMagnetBody :: Text -> FilePath -> Int -> Value
delugeAddMagnetBody magnet downloadDir rid =
  object [ "method" .= ("core.add_torrent_magnet" :: Text)
         , "params" .= [ Aeson.String magnet
                       , object ["download_location" .= downloadDir] ]
         , "id" .= rid ]

-- | Pure: Deluge core.add_torrent_file body (base64 filedump).
delugeAddFileBody :: Text -> Text -> FilePath -> Int -> Value
delugeAddFileBody name b64 downloadDir rid =
  object [ "method" .= ("core.add_torrent_file" :: Text)
         , "params" .= [ Aeson.String name, Aeson.String b64
                       , object ["download_location" .= downloadDir] ]
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

-- | Push a release to Deluge (magnet preferred, else download the
-- .torrent and add_torrent_file). On ANY failure, fall back to writing the
-- magnet / torrent bytes into the download dir so an external client or
-- the auto-import can pick it up. Returns Right () when the release is
-- either pushed or written to the download dir, Left err otherwise.
pushRequest
  :: Text       -- ^ deluge JSON-RPC url
  -> Text       -- ^ deluge password
  -> FilePath   -- ^ download dir
  -> Maybe Text -- ^ magnet
  -> Maybe Text -- ^ download (torrent) url
  -> Text       -- ^ title (for the fallback slug)
  -> IO (Either String ())
pushRequest delugeUrl delugePassword downloadDir mMagnet mDownloadUrl title = do
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
    newMgr = newManager (tlsManagerSettings
      { managerResponseTimeout = responseTimeoutMicro 10000000 })
    tryDeluge = do
      manager <- newMgr
      eLogin <- postJson manager delugeUrl Nothing (delugeLoginBody delugePassword 1)
      case eLogin of
        Left err -> pure (Left err)
        Right (body, cookie) ->
          case rpcResult body of
            Just res | rpcResultOk (Just res) -> case mMagnet of
              Just mag -> do
                er <- postJson manager delugeUrl cookie (delugeAddMagnetBody mag downloadDir 2)
                pure (rpcOk er)
              Nothing -> case mDownloadUrl of
                Just u -> do
                  ef <- fetchBytes u
                  case ef of
                    Left err -> pure (Left err)
                    Right bytes -> do
                      er <- postJson manager delugeUrl cookie
                        (delugeAddFileBody (T.pack (slug <.> "torrent")) (base64Encode bytes) downloadDir 2)
                      pure (rpcOk er)
                Nothing -> pure (Left "no magnet")
            _ -> pure (Left "deluge auth.login failed")
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
            ef <- fetchBytes u
            case ef of
              Left err -> pure (Left err)
              Right bytes -> do
                BS.writeFile (downloadDir </> (slug <.> "torrent")) bytes
                pure (Right ())
          Nothing -> pure (Left "no download source")
