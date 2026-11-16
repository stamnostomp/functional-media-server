module FMS.Modules.Playback.Routes
  ( playbackRoutes
  , playbackInfoHandler
  , mediaStreamsHandler
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import Network.Wai (Request, Response)
import System.Random (newStdGen, randomRs)

import FMS.Core.Module (Route, Segment(..), get, jsonResponse, post)
import FMS.Core.Monad (AppError(..), AppM, MonadLog(..))
import FMS.Core.Types.MediaItem (MediaItem(..))
import FMS.Modules.Library (MonadLibrary(..))
import FMS.Modules.Playback.Probe
  ( containerFromExtension
  , directPlayable
  , directStreamable
  , probeFile
  , transcoding
  )
import FMS.Modules.Playback.Types
  ( MediaSource(..)
  , MediaStream
  , PlaybackResponse(..)
  )

-- | The playback routes (docs/todo.org section 6, milestone 6a):
-- @POST /items/:id/PlaybackInfo@ (the Jellyfin playback-decision
-- endpoint; the request body is ignored — the default client profile
-- is assumed) and @GET /items/:id/MediaStreams@.
playbackRoutes :: [Route]
playbackRoutes =
  [ post [Lit "items", Capture "id", Lit "PlaybackInfo"] playbackInfoHandler
  , get  [Lit "items", Capture "id", Lit "MediaStreams"] mediaStreamsHandler
  ]

-- | @POST /items/:id/PlaybackInfo@ — probe the item's file with ffprobe
-- and answer with @{MediaSources: [...], PlaySessionId: "..."}@.
-- Unknown id -> 404. When ffprobe cannot read the file (corrupt /
-- dummy), the endpoint still answers 200 with a single MediaSource
-- that has no streams, the container guessed from the extension, and
-- SupportsTranscoding = True — graceful degradation instead of a 500,
-- so clients can always fall back to transcoding.
playbackInfoHandler :: [(Text, Text)] -> Request -> AppM Response
playbackInfoHandler captures _req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just i  -> do
      mItem <- getItem i
      case mItem of
        Nothing -> throwError (NotFound ("no library item with id " <> i))
        Just item -> do
          probed <- liftIO (probeFile (itemPath item))
          source <- mediaSourceFor item probed
          sid    <- liftIO newPlaySessionId
          pure (jsonResponse (PlaybackResponse [source] sid))

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
mediaStreamsHandler captures _req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just i  -> do
      mItem <- getItem i
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
