module FMS.Modules.Chapters
  ( chaptersModule
  , Chapter(..)
  , fallbackChapters
  , parseChaptersJson
  , probeChapters
  , probeChaptersArgs
  , chapterImageArgs
  , chapterImagePathFor
  , chapterToJson
  , ticksFromSeconds
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON(..), Value(..), withObject, (.:?), (.!=), (.=), object)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types (status200)
import Network.Wai (Request, Response, queryString, responseLBS)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), (<.>), takeDirectory)
import System.Process
  ( CreateProcess(..)
  , StdStream(..)
  , createProcess
  , proc
  , waitForProcess
  )
import Text.Read (readMaybe)

import FMS.Core.Module (Module(..), Segment(..), get, jsonResponse)
import FMS.Core.Monad (AppError(..), AppM, MonadLog(..), envConfig)
import FMS.Core.Process (runProcessBS, showBS)
import FMS.Core.Query (queryParams, versionParam, versionPathFor)
import FMS.Core.Types.Config (Config(..))
import FMS.Core.Types.MediaItem (MediaItem(..))
import FMS.Modules.Auth.Monad (getItemWithAccess)

-- | The chapter-images module (docs/todo.org section 7, milestone 6e):
-- @GET /items/:id/Chapters@ lists the item's chapters probed with ffprobe
-- (@-show_chapters@, falling back to 10 evenly spaced chapters when the
-- file carries none), and @GET /items/:id/Chapters/:index/Image@ serves
-- a JPEG frame at the chapter's start position (ffmpeg, cached under
-- @stateDir/chapters/<id>-<version>-<index>.jpg@). @?version=N@ picks
-- the media version exactly like the stream module (milestone 4j).
chaptersModule :: Module
chaptersModule = Module
  { moduleName   = "chapters"
  , moduleInit   = pure ()
  , moduleRoutes =
      [ get [Lit "items", Capture "id", Lit "Chapters"] chaptersHandler
      , get [Lit "items", Capture "id", Lit "Chapters", Capture "index", Lit "Image"]
          chapterImageHandler
      ]
  }

-- | One chapter: 0-based index, start position in seconds, optional
-- title (from ffprobe's @tags.title@).
data Chapter = Chapter
  { chIndex        :: Int
  , chStartSeconds :: Double
  , chTitle        :: Maybe Text
  } deriving (Eq, Show)

-- | Convert seconds to Jellyfin ticks (100 ns units).
ticksFromSeconds :: Double -> Integer
ticksFromSeconds s = round (s * 1e7)

-- | The Jellyfin-shaped JSON of a chapter:
-- @{"Index":i,"StartPositionTicks":t,"Name"?:..,"ImageTag":"i"}@.
chapterToJson :: Chapter -> Value
chapterToJson c = object $
  [ "Index" .= chIndex c
  , "StartPositionTicks" .= ticksFromSeconds (chStartSeconds c)
  , "ImageTag" .= T.pack (show (chIndex c))
  ] ++ [ "Name" .= t | Just t <- [chTitle c] ]

-- | Pure fallback when a file has no embedded chapters: @count@ evenly
-- spaced chapter starts over @duration@ seconds (titles 'Nothing').
-- @count <= 0@ is treated as the default 10; a non-positive duration
-- yields a single chapter at position 0.
fallbackChapters :: Double -> Int -> [Chapter]
fallbackChapters duration count
  | duration <= 0 = [Chapter 0 0 Nothing]
  | n <= 0        = fallbackChapters duration 10
  | otherwise     = [ Chapter i (fromIntegral i * duration / fromIntegral n) Nothing
                    | i <- [0 .. n - 1] ]
  where n = count

-- | The ffprobe argument list reading chapters + format duration of
-- @path@ as JSON on stdout. Pure so it is unit-testable.
probeChaptersArgs :: FilePath -> [String]
probeChaptersArgs path =
  [ "-v", "error"
  , "-print_format", "json"
  , "-show_chapters"
  , "-show_format"
  , path
  ]

-- | Run ffprobe with 'probeChaptersArgs' and parse the output. On a
-- non-zero exit the result is 'Left' (the handler falls back to a
-- single chapter at 0, since no duration is known); on a zero exit with
-- an empty chapters list the result falls back to 'fallbackChapters'
-- over the probed duration (10 chapters; duration 0 -> one chapter).
probeChapters :: FilePath -> IO (Either String (Double, [Chapter]))
probeChapters path = do
  (exitCode, out, err) <- runProcessBS "ffprobe" (probeChaptersArgs path)
  case exitCode of
    ExitFailure n ->
      pure (Left ("ffprobe exited with code " ++ show n ++ ": "
                  ++ take 300 (showBS (LBS.toStrict err))))
    ExitSuccess ->
      case parseChaptersJson out of
        Left e -> pure (Left e)
        Right (mDur, chs) ->
          let dur = maybe 0 id mDur
          in pure (Right (dur, if null chs then fallbackChapters dur 10 else chs))

-- | Pure parser for ffprobe's @-show_chapters -show_format@ JSON:
-- @(format duration, chapters)@. Each chapter's start is @start_time@
-- (seconds, string or number) and its title comes from @tags.title@;
-- the chapter index is its 0-based position in the list.
parseChaptersJson :: LBS.ByteString -> Either String (Maybe Double, [Chapter])
parseChaptersJson body = do
  doc <- Aeson.eitherDecode body :: Either String ChaptersDoc
  let mDur = cdFormat doc >>= cfDuration
      chs  = zipWith toChapter [0 ..] (cdChapters doc)
  pure (mDur, chs)
  where
    toChapter i ce = Chapter i (maybe 0 id (ceStartTime ce))
                           (ceTags ce >>= ctTitle)

-- Internal aeson shapes for the ffprobe chapters document.
data ChaptersDoc = ChaptersDoc
  { cdFormat   :: Maybe ChaptersFormat
  , cdChapters :: [ChapterEntry]
  }

instance FromJSON ChaptersDoc where
  parseJSON = withObject "ffprobe chapters output" $ \o ->
    ChaptersDoc
      <$> o .:? "format"
      <*> o .:? "chapters" .!= []

data ChaptersFormat = ChaptersFormat
  { cfDuration :: Maybe Double
  }

instance FromJSON ChaptersFormat where
  parseJSON = withObject "format" $ \o -> do
    dur <- o .:? "duration"
    pure (ChaptersFormat (dur >>= flexTime))

data ChapterEntry = ChapterEntry
  { ceStartTime :: Maybe Double
  , ceTags      :: Maybe ChapterTags
  }

instance FromJSON ChapterEntry where
  parseJSON = withObject "chapter" $ \o -> do
    st   <- o .:? "start_time"
    tags <- o .:? "tags"
    pure (ChapterEntry (st >>= flexTime) tags)

data ChapterTags = ChapterTags
  { ctTitle :: Maybe Text
  }

instance FromJSON ChapterTags where
  parseJSON = withObject "chapter tags" $ \o ->
    ChapterTags <$> o .:? "title"

-- | ffprobe emits times as JSON strings ("12.500000"); accept numbers
-- too (same newtype pattern as 'FMS.Modules.Playback.Probe').
newtype FlexTime = FlexTime (Maybe Double)

flexTime :: FlexTime -> Maybe Double
flexTime (FlexTime d) = d

instance FromJSON FlexTime where
  parseJSON (String t) = pure (FlexTime (readMaybe (T.unpack t)))
  parseJSON (Number n) = pure (FlexTime (Just (realToFrac n)))
  parseJSON _          = pure (FlexTime Nothing)

-- | The ffmpeg argument list extracting one JPEG frame at @start@
-- seconds from @path@ onto stdout (same shape as
-- 'FMS.Modules.Thumbs.thumbArgs'). Pure so it is unit-testable.
chapterImageArgs :: FilePath -> Double -> [String]
chapterImageArgs path start =
  [ "-v", "error"
  , "-ss", show start  -- ASCII-only: 'show' on a Double is locale-free
  , "-i", path
  , "-frames:v", "1"
  , "-q:v", "3"
  , "-f", "image2"
  , "-vcodec", "mjpeg"
  , "pipe:1"
  ]

-- | The per-chapter image cache path:
-- @stateDir \/ \"chapters\" \/ \<id\>-\<version\>-\<index\>.jpg@.
chapterImagePathFor :: FilePath -> Text -> Int -> Int -> FilePath
chapterImagePathFor stateDir i version idx =
  stateDir </> "chapters" </>
    (T.unpack i ++ "-" ++ show version ++ "-" ++ show idx <.> "jpg")

-- | @GET \/items\/:id\/Chapters@ — 404 for an unknown item, 400 for a
-- bad @version@ param. Probes the (version-selected) file and answers
-- the Jellyfin-shaped chapter array.
chaptersHandler :: [(Text, Text)] -> Request -> AppM Response
chaptersHandler captures req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just i  -> do
      mItem <- getItemWithAccess req i
      case mItem of
        Nothing -> throwError (NotFound ("no library item with id " <> i))
        Just item -> do
          path <- selectedPath item req
          r <- liftIO (probeChapters path)
          let (_dur, chs) = either (const (0, fallbackChapters 0 10)) id r
          pure (jsonResponse (map chapterToJson chs))

-- | @GET \/items\/:id\/Chapters\/:index\/Image@ — 404 unknown item, 400
-- invalid index or @version@, 404 index out of range. Serves the cached
-- JPEG when present, otherwise extracts the frame at the chapter start
-- with ffmpeg, caches it, and serves it as @image/jpeg@.
chapterImageHandler :: [(Text, Text)] -> Request -> AppM Response
chapterImageHandler captures req =
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
                path <- selectedPath item req
                r <- liftIO (probeChapters path)
                let (_dur, chs) = either (const (0, fallbackChapters 0 10)) id r
                if n >= length chs
                  then throwError (NotFound ("chapter index " <> T.pack (show n)
                                              <> " out of range for item " <> i))
                  else do
                    cfg <- asks envConfig
                    let v = either (const 0) id (versionParam (queryParams (queryString req)))
                        cachePath = chapterImagePathFor (configStateDir cfg) i v n
                        chapter = chs !! n
                    exists <- liftIO (doesFileExist cachePath)
                    if exists
                      then serveJpeg cachePath
                      else do
                        logInfo ("chapters: generating image for item " <> i
                                 <> " chapter " <> T.pack (show n))
                        gen <- liftIO (generateFrame
                                         (chapterImageArgs path (chStartSeconds chapter)))
                        case gen of
                          Left err -> throwError (InternalError err)
                          Right out
                            | BS.null out ->
                                throwError (InternalError
                                  ("ffmpeg produced an empty chapter image for item "
                                   <> i))
                            | otherwise -> do
                                liftIO (createDirectoryIfMissing True
                                          (takeDirectory cachePath))
                                liftIO (BS.writeFile cachePath out)
                                serveJpeg cachePath
          _ -> throwError (BadRequest "invalid chapter index")

-- | Resolve the @version@ query param against the item and return the
-- selected file path; a bad param or index is a 400.
selectedPath :: MediaItem -> Request -> AppM FilePath
selectedPath item req =
  case versionParam (queryParams (queryString req)) of
    Left e   -> throwError (BadRequest e)
    Right n  -> case versionPathFor item n of
      Left e   -> throwError (BadRequest e)
      Right p  -> pure p

-- | Run ffmpeg with the given args, capture stdout fully, and return the
-- JPEG bytes — or 'Left' when ffmpeg exits non-zero (same byte-pipe
-- pattern as 'FMS.Modules.Thumbs.generateThumb': 'BS.hGetContents', no
-- locale-decoded handles).
generateFrame :: [String] -> IO (Either Text BS.ByteString)
generateFrame args = do
  r <- createProcess (proc "ffmpeg" args) { std_out = CreatePipe }
  case r of
    (_, Just out, _, ph) -> do
      bs <- BS.hGetContents out
      ec <- waitForProcess ph
      pure $ case ec of
        ExitSuccess   -> Right bs
        ExitFailure n ->
          Left ("ffmpeg chapter image generation failed (exit "
                <> T.pack (show n) <> ")")
    _ -> pure (Left "ffmpeg stdout pipe was not created")

-- | Read a whole (cached) JPEG file and serve it as @image/jpeg@.
serveJpeg :: FilePath -> AppM Response
serveJpeg path = do
  bs <- liftIO (LBS.readFile path)
  pure (responseLBS status200 [("Content-Type", "image/jpeg")] bs)
