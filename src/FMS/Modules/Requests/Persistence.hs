module FMS.Modules.Requests.Persistence
  ( loadRequests
  , saveRequests
  , requestToPersist
  , requestFromPersist
  , loadRequestSettings
  , saveRequestSettings
  ) where

import Control.Exception (SomeException, try)
import qualified Data.Aeson as Aeson
import Data.Aeson (object, withObject, (.:), (.:?), (.!=), (.=))
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import System.Directory (doesFileExist)

import FMS.Modules.Requests.Types (Request (..), RequestSettings (..))

-- | Load the persisted request store (@stateDir/requests.json@): a bare
-- JSON object mapping request id -> request (LOWERCASE persisted keys).
-- A missing or unreadable file yields an empty map -- best-effort, never
-- fatal. Strict read ('BS.readFile', like the other modules).
loadRequests :: FilePath -> IO (HashMap Text Request)
loadRequests path = do
  exists <- doesFileExist path
  if not exists
    then pure HM.empty
    else do
      result <- try (BS.readFile path) :: IO (Either SomeException BS.ByteString)
      pure $ case result of
        Left _      -> HM.empty
        Right bytes ->
          case Aeson.decode (LBS.fromStrict bytes) :: Maybe (HashMap Text Aeson.Value) of
            Nothing -> HM.empty
            Just m  ->
              case traverse (parseEither requestFromPersist) m of
                Left _    -> HM.empty
                Right reqs -> reqs

-- | Persist the whole request map as a bare JSON object (lowercase keys).
saveRequests :: FilePath -> HashMap Text Request -> IO ()
saveRequests path reqs = LBS.writeFile path (Aeson.encode (HM.map requestToPersist reqs))

-- | Persistence JSON uses lowercase keys (unlike the PascalCase API JSON);
-- the two shapes are converted explicitly here so old files and API
-- payloads never mix.
requestToPersist :: Request -> Aeson.Value
requestToPersist r = object
  [ "id"            .= reqId r
  , "userId"        .= reqUserId r
  , "userName"      .= reqUserName r
  , "title"         .= reqTitle r
  , "year"          .= reqYear r
  , "tmdbId"        .= reqTmdbId r   -- m68
  , "season"        .= reqSeason r   -- m76
  , "episode"       .= reqEpisode r  -- m76
  , "mediaType"     .= reqMediaType r
  , "query"         .= reqQuery r
  , "status"        .= reqStatus r
  , "sizeBytes"     .= reqSizeBytes r
  , "sourceIndexer" .= reqSourceIndexer r
  , "sourceRelease" .= reqSourceRelease r
  , "sourceUrl"     .= reqSourceUrl r
  , "magnet"        .= reqMagnet r
  , "downloadUrl"   .= reqDownloadUrl r
  , "createdAt"     .= reqCreatedAt r
  , "updatedAt"     .= reqUpdatedAt r
  , "approvedBy"    .= reqApprovedBy r
  , "error"         .= reqError r
  , "downloadDir"   .= reqDownloadDir r   -- m27
  ]

requestFromPersist :: Aeson.Value -> Parser Request
requestFromPersist = withObject "request" $ \o ->
  Request
    <$> o .:  "id"
    <*> o .:  "userId"
    <*> o .:  "userName"
    <*> o .:  "title"
    <*> o .:? "year"
    <*> o .:? "tmdbId"        -- m68; old files without it load fine
    <*> o .:? "season"        -- m76; old files without it load fine
    <*> o .:? "episode"       -- m76
    <*> o .:? "mediaType"     .!= "movie"
    <*> o .:? "query"         .!= ""
    <*> o .:  "status"
    <*> o .:? "sizeBytes"
    <*> o .:? "sourceIndexer"
    <*> o .:? "sourceRelease"
    <*> o .:? "sourceUrl"
    <*> o .:? "magnet"
    <*> o .:? "downloadUrl"
    <*> o .:  "createdAt"
    <*> o .:  "updatedAt"
    <*> o .:? "approvedBy"
    <*> o .:? "error"
    <*> o .:? "downloadDir"   -- m27; old files without it load fine
    <*> o .:? "progress"      -- m37: live-only; never written, tolerated on read
    <*> o .:? "torrentState"

-- | Load the persisted download-client settings
-- (@stateDir/requests-settings.json@). A missing or unreadable file yields
-- 'Nothing' so the caller can seed from the env config (m27).
loadRequestSettings :: FilePath -> IO (Maybe RequestSettings)
loadRequestSettings path = do
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      result <- try (BS.readFile path) :: IO (Either SomeException BS.ByteString)
      pure $ case result of
        Left _      -> Nothing
        Right bytes -> Aeson.decode (LBS.fromStrict bytes)

-- | Persist the download-client settings (lowercase keys, strict write).
saveRequestSettings :: FilePath -> RequestSettings -> IO ()
saveRequestSettings path s = LBS.writeFile path (Aeson.encode s)
