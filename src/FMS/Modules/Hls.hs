module FMS.Modules.Hls
  ( hlsModule
  , hlsArgs
  , hlsDirFor
  , ensureHls
  , rewritePlaylistSegments
  , playlistHasSegments
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (IOException, try)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
-- bracket_ is IO-level; the AppM result handling happens after it returns
import Data.Char (isAlphaNum)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Network.HTTP.Types (status200)
import Network.Wai (Request, Response, queryString, responseLBS)
import System.Directory (createDirectory, createDirectoryIfMissing, doesFileExist, removeDirectory)
import System.FilePath ((</>), dropFileName)

import FMS.Core.Module (Module(..), Segment(..), get)
import FMS.Core.Monad (AppError(..), AppM, MonadLog(..), acquireFfmpegSlot, envConfig, releaseFfmpegSlot)
import FMS.Core.Process (runProcessBSTimeout)
import FMS.Core.Types.Config (Config(..))
import FMS.Core.Types.MediaItem (MediaItem(..))
import FMS.Modules.Auth.Monad (getItemWithAccess)

-- | The HLS module (docs/roadmap.org milestone 3c, docs/todo.org section 6):
-- Jellyfin-style HLS packaging, @GET /videos/:id/main.m3u8@ plus the
-- segment files @GET /videos/:id/:seg@. Segments are produced once per item
-- by ffmpeg into @stateDir/hls/<id>/@ and cached there; the playlist route
-- lazily triggers packaging on first request ('ensureHls'). Like the
-- stream module it only needs the 'MonadLibrary' capability — the core
-- never learns about it (docs/modules.org). ffmpeg is found on PATH at
-- runtime; the Nix package wraps the @fms@ binary with ffmpeg on its PATH
-- (flake.nix). Subprocess output is captured as raw bytes via
-- "FMS.Core.Process" — never locale-decoded — so a chatty ffmpeg can
-- never crash the server through a String pipe read.
hlsModule :: Module
hlsModule = Module
  { moduleName   = "hls"
  , moduleInit   = pure ()
  , moduleRoutes =
      -- The playlist route must come first: otherwise "main.m3u8" would
      -- also bind as a segment name in the capture route below.
      [ get [Lit "videos", Capture "id", Lit "main.m3u8"] playlistHandler
      , get [Lit "videos", Capture "id", Capture "seg"] segmentHandler
      ]
  }

-- | The ffmpeg argument list for progressive HLS packaging of @input@
-- into @outputM3u8@. Pure so it is unit-testable: stream-copy the codecs
-- (packaging is a remux, no re-encode), cut on ~4-second boundaries, and
-- keep every segment listed (@-hls_list_size 0@) so the playlist grows
-- segment-by-segment while ffmpeg runs and gets @#EXT-X-ENDLIST@ only at
-- the very end. That way playback can start after the first segment
-- instead of waiting for the whole file to be remuxed (m66d). Segments
-- land next to the playlist as @seg%03d.ts@.
hlsArgs :: FilePath -> FilePath -> [String]
hlsArgs input outputM3u8 =
  [ "-v", "error"
  , "-i", input
  -- map only video+audio: an embedded subtitle stream would be
  -- auto-selected and the HLS muxer rejects anything but webvtt
  -- ("Could not write header") -> no segments at all (m66e)
  , "-map", "0:v:0", "-map", "0:a?", "-sn", "-dn"
  , "-c", "copy"
  , "-hls_time", "4"
  , "-hls_list_size", "0"
  , "-hls_flags", "independent_segments"
  , "-hls_segment_filename", segPattern
  , outputM3u8
  ]
  where
    segPattern = dropFileName outputM3u8 </> "seg%03d.ts"

-- | How long the detached packaging ffmpeg may run (a remux always
-- terminates; this is just a safety net so a hung ffmpeg cannot hold an
-- ffmpeg slot forever). Generous because big files take a while even
-- stream-copied.
packagingTimeoutSec :: Int
packagingTimeoutSec = 3600

-- | How long a playlist request waits for the FIRST segment to appear
-- before giving up with a 500.
firstSegmentWaitSec :: Int
firstSegmentWaitSec = 60

-- | The per-item HLS cache directory: @stateDir \/ \"hls\" \/ \<id\>@.
hlsDirFor :: FilePath -> Text -> FilePath
hlsDirFor stateDir i = stateDir </> "hls" </> T.unpack i

-- | Make sure the HLS assets for @item@ exist under the state dir and
-- return the path of the m3u8 playlist. If @<dir>\/main.m3u8@ already
-- lists at least one segment, the cached/partial copy is served as-is
-- (no re-packaging). Otherwise ffmpeg is spawned DETACHED (m66d) under a
-- per-item @.starting@ lock so the first request returns as soon as the
-- first segment is listed -- the playlist keeps growing while ffmpeg
-- runs and hls.js treats it as a live playlist that ends with
-- @#EXT-X-ENDLIST@. A non-zero exit or no first segment within
-- 'firstSegmentWaitSec' becomes an 'InternalError' (500).
ensureHls :: Config -> MediaItem -> AppM FilePath
ensureHls cfg item = do
  let dir  = hlsDirFor (configStateDir cfg) (itemId item)
      m3u8 = dir </> "main.m3u8"
      lock = dir </> ".starting"
  ready <- liftIO (playlistHasSegments m3u8)
  if ready
    then pure m3u8
    else do
      liftIO (createDirectoryIfMissing True dir)
      r <- liftIO (try (createDirectory lock) :: IO (Either IOException ()))
      case r of
        Right () -> do
          -- we won the race: package in the background, keep an ffmpeg
          -- slot for the lifetime of the job
          logInfo ("hls: packaging item " <> itemId item <> " into " <> T.pack dir)
          mRelease <- acquireFfmpegSlot
          liftIO $ forkIO $ do
            _ <- runProcessBSTimeout packagingTimeoutSec
                   "ffmpeg" (hlsArgs (itemPath item) m3u8)
            releaseFfmpegSlot mRelease
            _ <- try (removeDirectory lock) :: IO (Either IOException ())
            pure ()
          ok <- liftIO (waitForFirstSegment firstSegmentWaitSec m3u8)
          if ok
            then pure m3u8
            else throwError (InternalError
                   ("ffmpeg HLS packaging produced no segments within "
                    <> T.pack (show firstSegmentWaitSec) <> "s for item "
                    <> itemId item))
        Left _ -> do
          -- another request is already packaging this item: just wait
          ok <- liftIO (waitForFirstSegment firstSegmentWaitSec m3u8)
          if ok
            then pure m3u8
            else throwError (InternalError
                   ("ffmpeg HLS packaging produced no segments within "
                    <> T.pack (show firstSegmentWaitSec) <> "s for item "
                    <> itemId item))

-- | Does the playlist file exist and list at least one @.ts@ segment?
playlistHasSegments :: FilePath -> IO Bool
playlistHasSegments m3u8 = do
  ok <- doesFileExist m3u8
  if not ok
    then pure False
    else do
      txt <- TIO.readFile m3u8
      pure (any (".ts" `T.isSuffixOf`) (T.lines txt))

-- | Poll until the playlist lists its first segment, at most @secs@.
waitForFirstSegment :: Int -> FilePath -> IO Bool
waitForFirstSegment secs m3u8 = go (secs * 5) -- 200ms steps
  where
    go 0 = pure False
    go n = do
      ready <- playlistHasSegments m3u8
      if ready then pure True else do
        threadDelay 200000
        go (n - 1)

-- | @GET \/videos\/:id\/main.m3u8@ — 404 for an unknown item, otherwise
-- ensure the HLS assets exist and serve the playlist file as
-- @application\/vnd.apple.mpegurl@.
playlistHandler :: [(Text, Text)] -> Request -> AppM Response
playlistHandler captures req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just i  -> do
      mItem <- getItemWithAccess req i
      case mItem of
        Nothing -> throwError (NotFound ("no library item with id " <> i))
        Just item -> do
          cfg  <- asks envConfig
          m3u8 <- ensureHls cfg item
          -- hls.js resolves relative segment URIs against the playlist
          -- URL and drops its query string, so segments would be
          -- requested without the api_key and get 401 under auth.
          -- Rewrite each segment line to carry the same credential the
          -- client used for the playlist itself.
          let mKey = case lookup "api_key" (queryString req) of
                Just (Just v) -> Just (TE.decodeUtf8 v)
                _             -> Nothing
          case mKey of
            Nothing -> serveFile m3u8 "application/vnd.apple.mpegurl"
            Just k  -> do
              txt <- liftIO (TIO.readFile m3u8)
              let out = rewritePlaylistSegments k txt
              pure (responseLBS status200
                [("Content-Type", "application/vnd.apple.mpegurl")]
                (LBS.fromStrict (TE.encodeUtf8 out)))

-- | @GET \/videos\/:id\/:seg@ — serve one cached MPEG-TS segment from
-- @stateDir\/hls\/<id>\/<seg>@ as @video\/mp2t@. Unknown item or missing
-- segment is 404; a segment name outside @[A-Za-z0-9._-]+@ (or containing
-- @..@) is rejected with 400 so it can never traverse out of the hls dir.
segmentHandler :: [(Text, Text)] -> Request -> AppM Response
segmentHandler captures req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just i  -> case lookup "seg" captures of
      Nothing -> throwError (BadRequest "missing seg capture")
      Just seg
        | not (isValidSegName seg) ->
            throwError (BadRequest ("invalid segment name: " <> seg))
        | otherwise -> do
            mItem <- getItemWithAccess req i
            case mItem of
              Nothing -> throwError (NotFound ("no library item with id " <> i))
              Just _item -> do
                cfg <- asks envConfig
                let segPath = hlsDirFor (configStateDir cfg) i </> T.unpack seg
                ok <- liftIO (doesFileExist segPath)
                if ok
                  then serveFile segPath "video/mp2t"
                  else throwError (NotFound ("no segment " <> seg <> " for item " <> i))

-- | Append @?api_key=<key>@ to every non-comment, non-empty playlist
-- line (the segment URIs) so HLS clients that resolve relative URIs
-- against the playlist URL still send the credential with each segment
-- request. Comment (@#...@) and blank lines pass through unchanged.
rewritePlaylistSegments :: Text -> Text -> Text
rewritePlaylistSegments key txt =
  T.unlines (map rewrite (T.lines txt))
  where
    rewrite line
      | T.null line = line
      | "#" `T.isPrefixOf` line = line
      | otherwise = line <> "?api_key=" <> key

-- | A segment name is safe to join onto the hls dir only if it is
-- non-empty, consists solely of @[A-Za-z0-9._-]@, and contains no @..@ —
-- so neither @..@ itself nor an embedded traversal can escape the dir.
isValidSegName :: Text -> Bool
isValidSegName s =
  not (T.null s)
  && T.all (\c -> isAlphaNum c || c == '.' || c == '_' || c == '-') s
  && not (".." `T.isInfixOf` s)

-- | Read a whole file and serve it with 'responseLBS' and the given
-- Content-Type — simple and safe for playlist/segment sizes.
serveFile :: FilePath -> BS.ByteString -> AppM Response
serveFile path contentType = do
  bs <- liftIO (LBS.readFile path)
  pure (responseLBS status200 [("Content-Type", contentType)] bs)
