module FMS.Modules.Thumbs
  ( thumbsModule
  , thumbArgs
  , thumbPathFor
  ) where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Types (status200)
import Network.Wai (Request, Response, responseLBS)
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

import FMS.Core.Module (Module(..), Segment(..), get)
import FMS.Core.Monad (AppError(..), AppM, MonadLog(..), envConfig)
import FMS.Core.Types.Config (Config(..))
import FMS.Core.Types.MediaItem (MediaItem(..))
import FMS.Modules.Auth.Monad (getItemWithAccess)

-- | The thumbnails module (docs/roadmap.org milestone 3e, docs/todo.org
-- section 5): @GET /items/:id/thumb@ serves a JPEG frame extracted from
-- the item's media file. The first request runs ffmpeg (frame at 5s) and
-- caches the JPEG under @stateDir/thumbs/<id>.jpg@; later requests serve
-- the cached file. Like the stream module it only needs the
-- 'MonadLibrary' capability — the core never learns about it
-- (docs/modules.org). ffmpeg is found on PATH at runtime; the Nix package
-- wraps the @fms@ binary with ffmpeg on its PATH (flake.nix).
thumbsModule :: Module
thumbsModule = Module
  { moduleName   = "thumbs"
  , moduleInit   = pure ()
  , moduleRoutes =
      [ get [Lit "items", Capture "id", Lit "thumb"] thumbHandler ]
  }

-- | The ffmpeg argument list extracting one JPEG frame (at 5 seconds) from
-- @path@ onto stdout. Pure so it is unit-testable: seek before @-i@ for
-- fast input seeking, take a single video frame (@-frames:v 1@), encode
-- it as MJPEG (@-vcodec mjpeg@) in an @image2@ container written to
-- @pipe:1@.
thumbArgs :: FilePath -> [String]
thumbArgs path =
  [ "-v", "error"
  , "-ss", "5"
  , "-i", path
  , "-frames:v", "1"
  , "-q:v", "3"
  , "-f", "image2"
  , "-vcodec", "mjpeg"
  , "pipe:1"
  ]

-- | The per-item thumbnail cache path: @stateDir \/ \"thumbs\" \/ \<id\>.jpg@.
thumbPathFor :: FilePath -> Text -> FilePath
thumbPathFor stateDir i = stateDir </> "thumbs" </> T.unpack i <.> ".jpg"

-- | @GET \/items\/:id\/thumb@ — 404 for an unknown item. If the cached
-- JPEG exists under the state dir it is served directly; otherwise ffmpeg
-- generates it, the result is written to the cache path, and it is served
-- as @image/jpeg@. A non-zero ffmpeg exit or empty output is an
-- 'InternalError' (500).
thumbHandler :: [(Text, Text)] -> Request -> AppM Response
thumbHandler captures req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just i  -> do
      mItem <- getItemWithAccess req i
      case mItem of
        Nothing -> throwError (NotFound ("no library item with id " <> i))
        Just item -> do
          cfg <- asks envConfig
          let cachePath = thumbPathFor (configStateDir cfg) i
          exists <- liftIO (doesFileExist cachePath)
          if exists
            then serveJpeg cachePath
            else do
              logInfo ("thumbs: generating thumbnail for item " <> i)
              r <- liftIO (generateThumb (itemPath item))
              case r of
                Left err -> throwError (InternalError err)
                Right out
                  | BS.null out ->
                      throwError (InternalError
                        ("ffmpeg produced an empty thumbnail for item " <> i))
                  | otherwise -> do
                      liftIO (createDirectoryIfMissing True (takeDirectory cachePath))
                      liftIO (BS.writeFile cachePath out)
                      serveJpeg cachePath

-- | Run ffmpeg with 'thumbArgs' on @path@, capture its stdout fully, and
-- return the JPEG bytes — or a 'Left' error message when ffmpeg exits
-- non-zero (or the pipe could not be created). Raw bytes are read with
-- 'BS.hGetContents', so no locale decoding issues arise.
generateThumb :: FilePath -> IO (Either Text BS.ByteString)
generateThumb path = do
  r <- createProcess (proc "ffmpeg" (thumbArgs path)) { std_out = CreatePipe }
  case r of
    (_, Just out, _, ph) -> do
      bs <- BS.hGetContents out
      ec <- waitForProcess ph
      pure $ case ec of
        ExitSuccess -> Right bs
        ExitFailure n ->
          Left ("ffmpeg thumbnail generation failed (exit "
                <> T.pack (show n) <> ") for " <> T.pack path)
    _ -> pure (Left "ffmpeg stdout pipe was not created")

-- | Read a whole (cached) JPEG file and serve it with 'responseLBS' as
-- @image/jpeg@.
serveJpeg :: FilePath -> AppM Response
serveJpeg path = do
  bs <- liftIO (LBS.readFile path)
  pure (responseLBS status200 [("Content-Type", "image/jpeg")] bs)
