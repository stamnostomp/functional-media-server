module FMS.Modules.Playback.Routes
  ( playbackRoutes
  , playbackInfoHandler
  , mediaStreamsHandler
  ) where

import Control.Monad (forM)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (FromJSON(..), eitherDecode, object, withObject, (.:?), (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Network.Wai (Request, Response, queryString, strictRequestBody)
import System.Directory (getFileSize)
import System.Random (newStdGen, randomRs)
import Text.Read (readMaybe)

import FMS.Core.Module (Route, Segment(..), get, jsonResponse, post)
import FMS.Core.Monad (AppError(..), AppM, MonadLog(..), envConfig)
import FMS.Core.Query (queryParams)
import FMS.Core.Types.Config (Config(..))
import FMS.Core.Types.MediaItem (MediaItem(..))
import FMS.Core.Types.Policy (UserPolicy(..))
import FMS.Core.Types.User (User(..))
import FMS.Modules.Auth.Monad (currentUserFromReq, getItemWithAccess)
import FMS.Modules.Playback.Probe
  ( containerFromExtension
  , decidePlayMethod
  , directPlayable
  , directStreamable
  , probeBitrate
  , probeFile
  , transcoding
  )
import FMS.Modules.Playback.Types
  ( MediaSource(..)
  , MediaStream
  , PlaybackResponse(..)
  )
import FMS.Modules.Transcode (effectiveBitrateCap)

-- | The playback routes (docs/todo.org section 6, milestone 6a):
-- @POST /items/:id/PlaybackInfo@ (the Jellyfin playback-decision
-- endpoint; milestone 6f: the optional body @{MaxStreamingBitrate?,
-- AudioStreamIndex?, SubtitleStreamIndex?, MaxWidth?}@ is honoured,
-- parse errors fall back to defaults) and @GET /items/:id/MediaStreams@.
playbackRoutes :: [Route]
playbackRoutes =
  [ post [Lit "items", Capture "id", Lit "PlaybackInfo"] playbackInfoHandler
  , get  [Lit "items", Capture "id", Lit "MediaStreams"] mediaStreamsHandler
  ]

-- | The optional @POST /items/:id/PlaybackInfo@ body (milestone 6f).
-- All fields optional; unknown keys (e.g. @MaxWidth@) are accepted and
-- ignored by aeson, and not modelled here.
data PlaybackInfoRequest = PlaybackInfoRequest
  { pirMaxBitrate    :: Maybe Int
  , pirAudioIndex    :: Maybe Int
  , pirSubtitleIndex :: Maybe Int
  }

instance FromJSON PlaybackInfoRequest where
  parseJSON = withObject "PlaybackInfoRequest" $ \o ->
    PlaybackInfoRequest
      <$> o .:? "MaxStreamingBitrate"
      <*> o .:? "AudioStreamIndex"
      <*> o .:? "SubtitleStreamIndex"

emptyPlaybackInfoRequest :: PlaybackInfoRequest
emptyPlaybackInfoRequest = PlaybackInfoRequest Nothing Nothing Nothing

-- | @POST /items/:id/PlaybackInfo@ — probe the item's file with ffprobe
-- and answer with @{MediaSources: [...], PlaySessionId: "..."}@.
-- Unknown id -> 404. When ffprobe cannot read the file (corrupt /
-- dummy), the endpoint still answers 200 with a single MediaSource
-- that has no streams, the container guessed from the extension, and
-- SupportsTranscoding = True — graceful degradation instead of a 500,
-- so clients can always fall back to transcoding.
playbackInfoHandler :: [(Text, Text)] -> Request -> AppM Response
playbackInfoHandler captures req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just i  -> do
      mItem <- getItemWithAccess req i
      case mItem of
        Nothing -> throwError (NotFound ("no library item with id " <> i))
        Just item -> do
          -- milestone 4j: one MediaSource per version path. Only the
          -- primary is probed; the other versions get the container from
          -- the extension and no streams (documented: probing every
          -- version on every PlaybackInfo would be expensive).
          let paths = if null (itemVersions item) then [itemPath item] else itemVersions item
          probed <- liftIO (probeFile (itemPath item))
          source0 <- mediaSourceFor item probed
          others <- forM (drop 1 paths) $ \p -> do
            sz <- liftIO (getFileSize p)
            pure MediaSource
              { sourceId             = itemId item
              , sourcePath           = p
              , container            = containerFromExtension p
              , sizeBytes            = sz
              , mediaStreams         = []
              , supportsDirectPlay   = False
              , supportsDirectStream = False
              , supportsTranscoding  = True
              }
          sid <- liftIO newPlaySessionId
          -- milestone 6f: optional client body (parse errors -> defaults)
          body <- liftIO (strictRequestBody req)
          let pir = either (const emptyPlaybackInfoRequest) id
                      (eitherDecode body :: Either String PlaybackInfoRequest)
          cfg <- asks envConfig
          mUser <- currentUserFromReq req
          let policyCap = mUser >>= policyMaxStreamingBitrate . userPolicy
              -- a ?maxBitrate= query param also counts as client-requested
              queryCap = lookup "maxbitrate" (queryParams (queryString req))
                          >>= \v -> readMaybe (T.unpack v)
              clientCap = case pirMaxBitrate pir of
                Just b  -> Just b
                Nothing -> queryCap
              cap = effectiveBitrateCap policyCap clientCap (configMaxBitrate cfg)
          srcBitrate <- liftIO (probeBitrate (itemPath item))
          let method = decidePlayMethod (supportsDirectPlay source0) srcBitrate cap
              tUrl =
                if method == "Transcode"
                  then Just (T.concat
                    [ "/stream/", i, "/transcode?version=0"
                    , maybe "" (\b -> "&maxBitrate=" <> T.pack (show b)) cap ])
                  else Nothing
          pure (jsonResponse PlaybackResponse
            { mediaSources        = source0 : others
            , playSessionId       = sid
            , playMethod          = method
            , maxStreamingBitrate = cap
            , transcodingUrl      = tUrl
            , audioStreamIndex    = pirAudioIndex pir
            , subtitleStreamIndex = pirSubtitleIndex pir
            })

-- | Build the 'MediaSource' for an item from the probe result: the
-- decision flags come from the pure logic in
-- "FMS.Modules.Playback.Probe"; on a probe failure the source is
-- empty and transcode-only.
mediaSourceFor :: MediaItem -> Either String (Text, [MediaStream]) -> AppM MediaSource
mediaSourceFor item probed =
  case probed of
    Right (c, streams) ->
      pure MediaSource
        { sourceId             = itemId item
        , sourcePath           = itemPath item
        , container            = c
        , sizeBytes            = itemSize item
        , mediaStreams         = streams
        , supportsDirectPlay   = directPlayable c streams
        , supportsDirectStream = directStreamable c streams
        , supportsTranscoding  = transcoding c streams
        }
    Left err -> do
      logErr (T.pack (unwords
        [ "ffprobe failed for", itemPath item ++ ":", err
        , "-- answering PlaybackInfo with an empty transcode-only MediaSource"
        ]))
      pure MediaSource
        { sourceId             = itemId item
        , sourcePath           = itemPath item
        , container            = containerFromExtension (itemPath item)
        , sizeBytes            = itemSize item
        , mediaStreams         = []
        , supportsDirectPlay   = False
        , supportsDirectStream = False
        , supportsTranscoding  = True
        }

-- | @GET /items/:id/MediaStreams@ — the probed streams of the item as
-- @{"MediaStreams": [...]}@. Unknown id -> 404; an unprobeable file
-- answers 200 with an empty list (same graceful policy as above).
mediaStreamsHandler :: [(Text, Text)] -> Request -> AppM Response
mediaStreamsHandler captures req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just i  -> do
      mItem <- getItemWithAccess req i
      case mItem of
        Nothing -> throwError (NotFound ("no library item with id " <> i))
        Just item -> do
          probed <- liftIO (probeFile (itemPath item))
          case probed of
            Right (_, streams) ->
              pure (jsonResponse (object ["MediaStreams" .= streams]))
            Left err -> do
              logErr (T.pack ("ffprobe failed for " ++ itemPath item ++ ": " ++ err
                             ++ " -- answering MediaStreams with []"))
              pure (jsonResponse (object ["MediaStreams" .= ([] :: [MediaStream])]))

-- | A fresh play session id: 16 random hex chars (same minting style as
-- the auth module's tokens, just shorter).
newPlaySessionId :: IO Text
newPlaySessionId = do
  g <- newStdGen
  let rs = take 16 (randomRs (0, 15) g) :: [Int]
  pure (T.pack (map (\r -> "0123456789abcdef" !! r) rs))
