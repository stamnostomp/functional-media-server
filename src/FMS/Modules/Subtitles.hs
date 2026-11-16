module FMS.Modules.Subtitles
  ( subtitlesModule
  , subtitleExts
  , pickSubtitles
  , subtitleToVttArgs
  , embeddedToVttArgs
  , SubtitleSource(..)
  , embeddedSubtitleSources
  , indexSources
  , subtitleListSources
  , subtitleList
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Data.Char (toLower)
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Builder (Builder, byteString)
import Data.List (sort)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types (status200)
import Network.Wai (Request, Response, responseLBS, responseStream)
import System.Directory (listDirectory)
import System.FilePath ((</>), takeBaseName, takeDirectory, takeExtension)

import FMS.Core.Module (Module(..), Segment(..), get, jsonResponse)
import FMS.Core.Monad (AppError(..), AppM)
import FMS.Core.Process (runProcessStreamBS)
import FMS.Core.Types.MediaItem (MediaItem(..))
import FMS.Modules.Auth.Monad (getItemWithAccess)
import FMS.Modules.Playback.Probe (probeFile)
import FMS.Modules.Playback.Types (MediaStream(..))

-- | The subtitles module (docs/roadmap.org milestone 3d, docs/todo.org
-- section 7): detect external subtitle files sitting next to a media
-- item, list them, and serve them as WebVTT — raw when the file already
-- is @.vtt@, otherwise converted on the fly by ffmpeg. Milestone 3f adds
-- embedded subtitle streams (found via ffprobe, 'FMS.Modules.Playback.Probe')
-- alongside the external files. Routes:
--
-- * @GET /items/:id/subtitles@ — JSON array of subtitle entries (external
--   @{\"index\",\"file\",\"format\"}@ first, then embedded
--   @{\"index\",\"format\",\"embedded\":true,\"streamIndex\",\"language\"?}@)
-- * @GET /stream/:id/subtitle/:index@ — that subtitle as @text/vtt@
--
-- Like the stream module it only needs the 'MonadLibrary' capability —
-- the core never learns about it (docs/modules.org). ffmpeg is found on
-- PATH at runtime; the Nix package wraps the @fms@ binary with ffmpeg on
-- its PATH (flake.nix).
subtitlesModule :: Module
subtitlesModule = Module
  { moduleName   = "subtitles"
  , moduleInit   = pure ()
  , moduleRoutes =
      [ get [Lit "items", Capture "id", Lit "subtitles"] listSubtitlesHandler
      , get [Lit "stream", Capture "id", Lit "subtitle", Capture "index"]
          serveSubtitleHandler
      ]
  }

-- | Recognised external subtitle extensions, lowercase (compared
-- case-insensitively in 'pickSubtitles').
subtitleExts :: [String]
subtitleExts = [".srt", ".vtt", ".ass", ".ssa", ".sub"]

-- | Given the media file path and the entries of its directory, return
-- the matching subtitle file paths (absolute, joined onto the media's
-- directory), always sorted so the indexes are stable. Files whose
-- basename matches the media basename (@takeBaseName media <ext>@) are
-- preferred; when none match, every subtitle file in the directory is
-- returned. Pure so it is unit-testable.
pickSubtitles :: FilePath -> [FilePath] -> [FilePath]
pickSubtitles media entries =
  let dir  = takeDirectory media
      base = map toLower (takeBaseName media)
      isSub e = map toLower (takeExtension e) `elem` subtitleExts
      subs = filter isSub entries
      matching = filter (\e -> map toLower (takeBaseName e) == base) subs
      chosen = if null matching then subs else matching
  in sort (map (dir </>) chosen)

-- | The ffmpeg argument list converting the subtitle file at @path@ to
-- WebVTT on stdout. Pure so it is unit-testable.
subtitleToVttArgs :: FilePath -> [String]
subtitleToVttArgs path =
  [ "-v", "error"
  , "-i", path
  , "-f", "webvtt"
  , "pipe:1"
  ]

-- | Milestone 3f: a subtitle source of a media file is either an
-- external subtitle file next to the media, or a stream embedded in the
-- media itself (found via ffprobe).
data SubtitleSource
  = External FilePath String            -- ^ file path, format (lowercase ext without the dot)
  | Embedded Int String (Maybe Text)    -- ^ ffprobe stream index, codec name, optional language
  deriving (Show, Eq)

-- | Pure (milestone 3f): the embedded subtitle streams of a probe result
-- as @(streamIndex, codec, language)@ triples — only streams whose
-- 'msType' is @\"Subtitle\"@ (see 'streamTypeName').
embeddedSubtitleSources :: [MediaStream] -> [(Int, String, Maybe Text)]
embeddedSubtitleSources streams =
  [ (msIndex s, T.unpack (msCodec s), msLanguage s)
  | s <- streams, msType s == "Subtitle"
  ]

-- | Pure: zip sources with their 0-based indexes — the combined list
-- used by both handlers (externals first, then embedded, so the indexes
-- of the external entries are unchanged from milestone 3d).
indexSources :: [SubtitleSource] -> [(Int, SubtitleSource)]
indexSources = zip [0 ..]

-- | IO: all subtitle sources of a media file — the external files picked
-- by 'pickSubtitles' first, then the embedded streams reported by
-- 'probeFile'. A probe failure yields no embedded entries and never
-- crashes (the external list is still returned).
subtitleListSources :: FilePath -> IO [SubtitleSource]
subtitleListSources media = do
  entries <- listDirectory (takeDirectory media)
  let exts = [ External p (formatOf p) | p <- pickSubtitles media entries ]
  probe <- probeFile media
  let emb = case probe of
        Right (_, streams) ->
          [ Embedded n c l | (n, c, l) <- embeddedSubtitleSources streams ]
        Left _ -> []
  pure (exts ++ emb)
  where
    formatOf p = drop 1 (map toLower (takeExtension p))

-- | Backward-compatible (milestone 3d): the external subtitle files of a
-- media file as @(index, path, format)@ — the external prefix of
-- 'subtitleListSources', with the same 0-based indexes as before.
subtitleList :: FilePath -> IO [(Int, FilePath, String)]
subtitleList media = do
  srcs <- subtitleListSources media
  pure [ (n, p, fmt) | (n, External p fmt) <- indexSources srcs ]

-- | Milestone 3f: the ffmpeg argument list extracting the embedded stream
-- with ffprobe index @n@ of @media@ and converting it to WebVTT on
-- stdout. Pure so it is unit-testable.
embeddedToVttArgs :: FilePath -> Int -> [String]
embeddedToVttArgs media n =
  [ "-v", "error"
  , "-i", media
  , "-map", "0:" ++ show n
  , "-c:s", "webvtt"
  , "-f", "webvtt"
  , "pipe:1"
  ]

-- | @GET \/items\/:id\/subtitles@ — 404 for an unknown item, otherwise a
-- JSON array of the item's subtitle sources, external files first and
-- embedded streams after (milestone 3f). External entries keep the
-- milestone-3d shape @{\"index\",\"file\",\"format\"}@; embedded entries
-- are @{\"index\",\"format\": codec, \"embedded\": true, \"streamIndex\": n,
-- \"language\"?: lang}@ and carry NO @file@ key — the subtitle lives
-- inside the media file at the item's @path@.
listSubtitlesHandler :: [(Text, Text)] -> Request -> AppM Response
listSubtitlesHandler captures req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just i  -> do
      mItem <- getItemWithAccess req i
      case mItem of
        Nothing -> throwError (NotFound ("no library item with id " <> i))
        Just item -> do
          srcs <- liftIO (subtitleListSources (itemPath item))
          pure (jsonResponse
            [ case s of
                External p fmt ->
                  object
                    [ "index"  .= n
                    , "file"   .= p
                    , "format" .= fmt
                    ]
                Embedded si codec mLang ->
                  object
                    ([ "index"       .= n
                     , "format"      .= codec
                     , "embedded"    .= True
                     , "streamIndex" .= si
                     ] ++ maybe [] (\l -> ["language" .= l]) mLang)
            | (n, s) <- indexSources srcs
            ])

-- | @GET \/stream\/:id\/subtitle\/:index@ — serve one subtitle source of
-- the item as @text/vtt@. The @index@ selects from the combined source
-- list (externals first, then embedded streams — milestone 3f). Unknown
-- item is 404; an @index@ that is not an integer or is out of range is
-- 400. An external @.vtt@ file is served raw; any other external file is
-- converted to WebVTT by ffmpeg; an embedded stream is extracted with
-- @-map 0:<streamIndex>@ and converted to WebVTT, both streamed.
serveSubtitleHandler :: [(Text, Text)] -> Request -> AppM Response
serveSubtitleHandler captures req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just i  -> case lookup "index" captures of
      Nothing -> throwError (BadRequest "missing index capture")
      Just idxTxt ->
        case reads (T.unpack idxTxt) :: [(Int, String)] of
          [(n, "")] | n >= 0 -> do
            mItem <- getItemWithAccess req i
            case mItem of
              Nothing -> throwError (NotFound ("no library item with id " <> i))
              Just item -> do
                srcs <- liftIO (subtitleListSources (itemPath item))
                case [ s | (n', s) <- indexSources srcs, n' == n ] of
                  [] -> throwError (BadRequest
                    ("subtitle index " <> idxTxt <> " out of range ("
                     <> T.pack (show (length srcs)) <> " subtitle(s) for item " <> i <> ")"))
                  (src : _) -> case src of
                    External path _fmt
                      | map toLower (takeExtension path) == ".vtt" -> do
                          bs <- liftIO (LBS.readFile path)
                          pure (responseLBS status200 [("Content-Type", "text/vtt")] bs)
                      | otherwise ->
                          pure (responseStream status200
                                  [("Content-Type", "text/vtt")]
                                  (streamToVtt path))
                    Embedded si _ _ ->
                      pure (responseStream status200
                              [("Content-Type", "text/vtt")]
                              (streamEmbeddedVtt (itemPath item) si))
          _ -> throwError (BadRequest ("invalid subtitle index: " <> idxTxt))

-- | The 'Network.Wai.StreamingBody' used with 'responseStream': run
-- ffmpeg converting @path@ to WebVTT and write its stdout to the WAI sink
-- in chunks of 64 KiB, like 'FMS.Modules.Transcode.streamTranscode'.
-- 'runProcessStreamBS' streams stdout chunk-by-chunk and drains stderr as
-- raw bytes concurrently (never locale-decoded), and it always closes
-- the pipes and reaps ffmpeg — even if the client disconnects mid-stream
-- and the sink throws.
streamToVtt
  :: FilePath
  -> (Builder -> IO ()) -> IO () -> IO ()
streamToVtt path send flush = do
  _ <- runProcessStreamBS "ffmpeg" (subtitleToVttArgs path) (send . byteString)
  flush

-- | Milestone 3f: like 'streamToVtt', but extracting the embedded stream
-- with ffprobe index @n@ of @media@ (see 'embeddedToVttArgs') instead of
-- converting an external file.
streamEmbeddedVtt
  :: FilePath
  -> Int
  -> (Builder -> IO ()) -> IO () -> IO ()
streamEmbeddedVtt media n send flush = do
  _ <- runProcessStreamBS "ffmpeg" (embeddedToVttArgs media n) (send . byteString)
  flush
