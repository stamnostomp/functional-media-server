module FMS.Modules.Hls
  ( hlsModule
  , hlsArgs
  , hlsDirFor
  , ensureHls
  , rewritePlaylistSegments
  ) where

import Control.Exception (bracket_)
import Control.Monad (unless)
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
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), dropFileName)

import FMS.Core.Module (Module(..), Segment(..), get)
import FMS.Core.Monad (AppError(..), AppM, MonadLog(..), acquireFfmpegSlot, envConfig, releaseFfmpegSlot)
import FMS.Core.Process (runProcessBSTimeout, showBS)
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

-- | The ffmpeg argument list for HLS VOD packaging of @input@ into
-- @outputM3u8@. Pure so it is unit-testable: stream-copy the codecs
-- (packaging is a remux, no re-encode), cut on 4-second boundaries, write
-- a VOD playlist (complete, with @#EXT-X-ENDLIST@), and place the
-- MPEG-TS segments next to the playlist as @seg%03d.ts@.
hlsArgs :: FilePath -> FilePath -> [String]
hlsArgs input outputM3u8 =
  [ "-v", "error"
  , "-i", input
  , "-c", "copy"
  , "-hls_time", "4"
  , "-hls_playlist_type", "vod"
  , "-hls_segment_filename", segPattern
  , outputM3u8
  ]
  where
    segPattern = dropFileName outputM3u8 </> "seg%03d.ts"

-- | The per-item HLS cache directory: @stateDir \/ \"hls\" \/ \<id\>@.
hlsDirFor :: FilePath -> Text -> FilePath
hlsDirFor stateDir i = stateDir </> "hls" </> T.unpack i

-- | Make sure the HLS assets for @item@ exist under the state dir and
-- return the path of the m3u8 playlist. If @<dir>\/main.m3u8@ is already
-- there the cached copy is reused (no re-packaging); otherwise ffmpeg runs
-- synchronously and a non-zero exit becomes an 'InternalError' (500).
-- Presence is the staleness check for now (docs/roadmap.org milestone 3c).
ensureHls :: Config -> MediaItem -> AppM FilePath
ensureHls cfg item = do
  let dir  = hlsDirFor (configStateDir cfg) (itemId item)
      m3u8 = dir </> "main.m3u8"
  exists <- liftIO (doesFileExist m3u8)
  unless exists $ do
    liftIO (createDirectoryIfMissing True dir)
    logInfo ("hls: packaging item " <> itemId item <> " into " <> T.pack dir)
    -- milestone 15: run packaging under the ffmpeg slot semaphore (503 on
    -- exhaustion) with the wall-clock killer timeout; the slot is released
    -- when the job ends either way
    mRelease <- acquireFfmpegSlot
    r <- liftIO $ bracket_ (pure ()) (releaseFfmpegSlot mRelease)
           (runProcessBSTimeout (configFfmpegTimeoutSec cfg)
              "ffmpeg" (hlsArgs (itemPath item) m3u8))
    case r of
      (ExitSuccess, _out, _err) -> pure ()
      (ExitFailure n, _out, err) ->
        throwError (InternalError
          ("ffmpeg HLS packaging failed (exit " <> T.pack (show n) <> "): "
           <> T.pack (take 2000 (showBS (LBS.toStrict err)))))
  pure m3u8

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
