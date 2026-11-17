module FMS.Modules.Images
  ( imagesModule
  , imageExtFor
  , isValidImageType
  , resizeArgs
  , imagePathFor
  , imagesDirFor
  , listImageTypes
  , findStoredImage
  , imageTag
  , imageTagOfFile
  , selectBackdrop
  , blurhashForStoredImage
  , blurhashPairsForItem
  , hasStoredImage
  , storeImageBytes
  ) where

import Control.Monad (forM, unless, when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVarIO)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))
import qualified Data.Aeson as Aeson
import Data.CaseInsensitive (mk)
import Data.Char (isDigit, toLower)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.List (isPrefixOf, nub, sort, stripPrefix)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Text (Text)
import Data.Word (Word8)
import qualified Data.Text as T
import Network.HTTP.Types (ResponseHeaders, hContentType, status200)
import Network.Wai
  ( Request, Response, queryString, requestHeaders, responseLBS
  , strictRequestBody
  )
import System.Directory
  ( createDirectoryIfMissing, doesDirectoryExist, doesFileExist
  , getFileSize, getModificationTime, listDirectory, removeFile
  )
import System.Exit (ExitCode(..))
import System.FilePath ((</>), takeBaseName, takeDirectory, takeExtension)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime(..), diffUTCTime)

import FMS.Core.Module (Module(..), Segment(..), delete, get, jsonResponse, post)
import FMS.Core.Monad (AppError(..), AppM, MonadLog(..), envConfig, getState, putState)
import FMS.Core.Blurhash (encodeBlurhash)
import FMS.Core.Process (runProcessBS, runProcessBSTimeout, showBS)
import FMS.Core.Query (parseBoundedInt, queryParams)
import FMS.Core.Types.Config (Config(..))
import FMS.Modules.Auth.Monad (getItemWithAccess)
import FMS.Modules.Thumbs (thumbPathFor)

-- | The images module (docs/roadmap.org milestone 6b, docs/todo.org
-- section 5): per-item images in the Jellyfin shape
-- @/items/:id/images[/:type]@ — list, serve (with ffmpeg resize via
-- @maxWidth@/@maxHeight@/@quality@), upload (POST raw bytes), and
-- delete. Stored uploads live under @stateDir/images/<id>/<type>.<ext>@;
-- resized variants are cached next to them as
-- @<type>-<w>x<h>.jpg@. @Primary@ and @Thumb@ fall back to the thumbs
-- cache (@stateDir/thumbs/<id>.jpg@, see "FMS.Modules.Thumbs") when no
-- stored file exists. Like the other media modules it only needs the
-- 'MonadLibrary' capability — the core never learns about it
-- (docs/modules.org). ffmpeg is found on PATH at runtime; the Nix
-- package wraps the @fms@ binary with ffmpeg on its PATH (flake.nix).
-- All subprocess output is captured as raw bytes via
-- "FMS.Core.Process" — never locale-decoded.
imagesModule :: Module
imagesModule = Module
  { moduleName   = "images"
  , moduleInit   = pure ()
  , moduleRoutes =
      [ get    [Lit "items", Capture "id", Lit "images"] listImagesHandler
      , get    [Lit "items", Capture "id", Lit "images", Capture "type"]
               getImageHandler
      , get    [Lit "items", Capture "id", Lit "images", Capture "type", Capture "index"]
               backdropIndexHandler
      , post   [Lit "items", Capture "id", Lit "images", Capture "type"]
               uploadImageHandler
      , delete [Lit "items", Capture "id", Lit "images", Capture "type"]
               deleteImageHandler
      ]
  }

-- | The per-item image store directory: @stateDir \/ \"images\" \/ \<id\>@.
imagesDirFor :: FilePath -> Text -> FilePath
imagesDirFor stateDir i = stateDir </> "images" </> T.unpack i

-- | Pure cache-busting tag for an image: @<size>-<mtimeEpochSeconds>@.
-- Size alone is not enough (a re-encode can keep the size), so the
-- modification time (epoch seconds) is included. Deterministic, so
-- unit-testable on synthetic pairs (milestone 6g).
imageTag :: Integer -> Int -> Text
imageTag size epoch = T.pack (show size ++ "-" ++ show epoch)

-- | The 'imageTag' of an actual file: size + mtime epoch seconds.
imageTagOfFile :: FilePath -> IO Text
imageTagOfFile path = do
  size <- getFileSize path
  mtime <- getModificationTime path
  let epoch = round (diffUTCTime mtime (UTCTime (fromGregorian 1970 1 1) 0)) :: Int
  pure (imageTag size epoch)

-- | Pure backdrop selection (milestone 6g): @index@ into the sorted list
-- of backdrop files; out of range (or negative) -> 'Nothing'.
selectBackdrop :: [FilePath] -> Int -> Maybe FilePath
selectBackdrop files n
  | n < 0 || n >= length files = Nothing
  | otherwise = Just (sort files !! n)

-- | The path of a stored image: @stateDir \/ \"images\" \/ \<id\> \/ \<type\>\<ext\>@.
imagePathFor :: FilePath -> Text -> Text -> String -> FilePath
imagePathFor stateDir i ty ext =
  imagesDirFor stateDir i </> (T.unpack ty ++ ext)

-- | The cached resize-variant path for a request:
-- @\<dir\>\/\<type\>-\<w\>x\<h\>.jpg@ (a missing dimension is recorded as
-- @auto@, e.g. @Primary-50xauto.jpg@ for @?maxWidth=50@).
variantPathFor :: FilePath -> Text -> Maybe Int -> Maybe Int -> FilePath
variantPathFor dir ty mW mH =
  dir </> (T.unpack ty ++ "-" ++ w ++ "x" ++ h ++ ".jpg")
  where
    w = maybe "auto" show mW
    h = maybe "auto" show mH

-- | An image type is a non-empty run of ASCII letters only
-- (@Primary@, @Backdrop@, @Logo@, ...). Restricting to @[A-Za-z]+@ also
-- makes path traversal impossible: the type becomes a file name
-- component verbatim.
isValidImageType :: Text -> Bool
isValidImageType t =
  not (T.null t)
  && T.all (\c -> (c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')) t

-- | The file extension for an upload, derived from the request's
-- @Content-Type@ header (@imageExtFor \"image\/png\" == \".png\"@).
-- Parameters are stripped (@image\/png; charset=binary@ works); unknown
-- or missing content types default to @.jpg@.
imageExtFor :: BS.ByteString -> String
imageExtFor ct =
  case T.toLower (T.strip (T.pack (BSC.unpack (BSC.takeWhile (/= ';') ct)))) of
    "image/png"  -> ".png"
    "image/jpeg" -> ".jpg"
    "image/jpg"  -> ".jpg"
    "image/webp" -> ".webp"
    _            -> ".jpg"

-- | The Content-Type for a stored file, from its extension.
contentTypeForExt :: String -> BS.ByteString
contentTypeForExt ext = case map toLower ext of
  ".png"  -> "image/png"
  ".jpg"  -> "image/jpeg"
  ".jpeg" -> "image/jpeg"
  ".webp" -> "image/webp"
  _       -> "application/octet-stream"

-- | The ffmpeg argument list resizing @input@ to at most @mW@ x @mH@
-- (either may be 'Nothing') at JPEG quality @q@, writing @output@.
-- Pure so it is unit-testable. Both bounds -> scale with
-- @force_original_aspect_ratio=decrease@ (never upscale beyond the
-- original, aspect preserved); a single bound -> the other dimension is
-- derived (@-2@ keeps it even, as JPEG requires).
resizeArgs :: FilePath -> Maybe Int -> Maybe Int -> Int -> FilePath -> [String]
resizeArgs input mW mH quality output =
  [ "-v", "error"
  , "-i", input
  , "-vf", scaleExpr
  , "-q:v", show quality
  , "-y", output
  ]
  where
    scaleExpr = case (mW, mH) of
      (Just w, Just h) ->
        "scale=" ++ show w ++ ":" ++ show h ++ ":force_original_aspect_ratio=decrease"
      (Just w, Nothing) -> "scale=" ++ show w ++ ":-2"
      (Nothing, Just h) -> "scale=-2:" ++ show h
      (Nothing, Nothing) -> "scale=iw:ih"  -- no-op; handler never resizes then

-- | Default JPEG quality for resized variants (Jellyfin's default).
defaultQuality :: Int
defaultQuality = 90

-- | @GET \/items\/:id\/images@ — 404 for an unknown item, otherwise 200
-- with a JSON array of @{\"Type\": t, \"Tag\"?: tag}@ entries (milestone
-- 6g): the stored files in @stateDir/images/<id>/@ (originals only —
-- resized variants are filtered out by 'isValidImageType'), plus @Thumb@
-- and @Primary@ when the thumbs cache exists. @Tag@ is the cache-busting
-- 'imageTagOfFile' of the backing file when it exists.
listImagesHandler :: [(Text, Text)] -> Request -> AppM Response
listImagesHandler captures req =
  case lookup "id" captures of
    Nothing -> throwError (BadRequest "missing id capture")
    Just i  -> do
      mItem <- getItemWithAccess req i
      case mItem of
        Nothing -> throwError (NotFound ("no library item with id " <> i))
        Just _  -> do
          cfg   <- asks envConfig
          let stateDir = configStateDir cfg
          types <- liftIO (listImageTypes stateDir i)
          entries <- forM types $ \ty -> do
            mStored <- liftIO (findStoredImage (imagesDirFor stateDir i) ty)
            mTag <- case mStored of
              Just (p, _) -> Just <$> liftIO (imageTagOfFile p)
              Nothing
                | ty == "Primary" || ty == "Thumb" -> do
                    let thumb = thumbPathFor stateDir i
                    ok <- liftIO (doesFileExist thumb)
                    if ok then Just <$> liftIO (imageTagOfFile thumb) else pure Nothing
                | otherwise -> pure Nothing
            pure (object $
              ["Type" .= ty] ++ [ "Tag" .= t | Just t <- [mTag] ])
          pure (jsonResponse entries)

-- | The image type names available for an item: stored originals plus
-- the thumb-cache fallbacks.
listImageTypes :: FilePath -> Text -> IO [Text]
listImageTypes stateDir i = do
  let dir = imagesDirFor stateDir i
  dirExists <- doesDirectoryExist dir
  stored <- if dirExists
    then do
      entries <- listDirectory dir
      pure [ s | e <- sort entries, let s = T.pack (takeBaseName e)
           , isValidImageType s ]
    else pure []
  thumbExists <- doesFileExist (thumbPathFor stateDir i)
  let fallbacks = if thumbExists then ["Thumb", "Primary"] else []
  pure (nub (sort (stored ++ map T.pack fallbacks)))

-- | @GET \/items\/:id\/images\/:type@ — serve one image. An invalid type
-- (not @[A-Za-z]+@) is 400; an unknown item is 404. The stored file
-- @stateDir/images/<id>/<type>.<ext>@ is served with the content type
-- of its extension, plus an @ETag@ (the 'imageTagOfFile' cache tag,
-- milestone 6g) and @Cache-Control: public, max-age=86400@ headers. A
-- @?tag=@ query param is accepted for URL cache-busting but never
-- changes the status (always 200 when the image exists — documented).
-- When @maxWidth@ or @maxHeight@ is present the image is resized with
-- ffmpeg into the cached variant @<type>-<w>x<h>.jpg@ (served as
-- @image\/jpeg@; a non-zero ffmpeg exit is a 500). Without a stored
-- file, @Primary@\/@Thumb@ fall back to the cached thumbnail
-- @stateDir/thumbs/<id>.jpg@; anything else is 404.
getImageHandler :: [(Text, Text)] -> Request -> AppM Response
getImageHandler captures req =
  case (lookup "id" captures, lookup "type" captures) of
    (Nothing, _) -> throwError (BadRequest "missing id capture")
    (_, Nothing) -> throwError (BadRequest "missing type capture")
    (Just i, Just ty)
      | not (isValidImageType ty) ->
          throwError (BadRequest ("invalid image type: " <> ty))
      | otherwise -> do
          mItem <- getItemWithAccess req i
          case mItem of
            Nothing -> throwError (NotFound ("no library item with id " <> i))
            Just _ -> do
              cfg <- asks envConfig
              let stateDir = configStateDir cfg
                  dir      = imagesDirFor stateDir i
              mStored <- liftIO (findStoredImage dir ty)
              case mStored of
                Just (path, ext) -> serveStoredImage dir i ty path ext req
                Nothing
                  | ty == "Primary" || ty == "Thumb" -> do
                      let thumb = thumbPathFor stateDir i
                      ok <- liftIO (doesFileExist thumb)
                      if ok
                        then do
                          tag <- liftIO (imageTagOfFile thumb)
                          serveImageFileWithHeaders thumb "image/jpeg"
                            [(mk "ETag", tagB tag), (mk "Cache-Control", "public, max-age=86400")]
                        else throwError (NotFound
                               ("no image of type " <> ty <> " for item " <> i))
                  | otherwise ->
                      throwError (NotFound
                        ("no image of type " <> ty <> " for item " <> i))

-- | @GET \/items\/:id\/images\/:type\/:index@ — backdrop index selection
-- (milestone 6g). For @Backdrop@ (case-insensitive) the index selects
-- among the stored backdrop files (@Backdrop.jpg@, @Backdrop1.jpg@, ...
-- sorted) via 'selectBackdrop'; out of range -> 404. For any other type,
-- index 0 behaves exactly like the base route and index > 0 is 404.
backdropIndexHandler :: [(Text, Text)] -> Request -> AppM Response
backdropIndexHandler captures req =
  case (lookup "id" captures, lookup "type" captures, lookup "index" captures) of
    (Nothing, _, _) -> throwError (BadRequest "missing id capture")
    (_, Nothing, _) -> throwError (BadRequest "missing type capture")
    (_, _, Nothing) -> throwError (BadRequest "missing index capture")
    (Just i, Just ty, Just idxTxt)
      | not (isValidImageType ty) ->
          throwError (BadRequest ("invalid image type: " <> ty))
      | otherwise ->
          case reads (T.unpack idxTxt) :: [(Int, String)] of
            [(n, "")] | n >= 0 -> do
              mItem <- getItemWithAccess req i
              case mItem of
                Nothing -> throwError (NotFound ("no library item with id " <> i))
                Just _ -> do
                  cfg <- asks envConfig
                  let stateDir = configStateDir cfg
                      dir      = imagesDirFor stateDir i
                  if T.toLower ty == "backdrop"
                    then do
                      files <- liftIO (backdropFiles dir)
                      case selectBackdrop files n of
                        Just p  -> serveStoredImage dir i ty p (takeExtension p) req
                        Nothing -> throwError (NotFound
                          ("no backdrop index " <> T.pack (show n)
                           <> " for item " <> i))
                    else
                      if n == 0
                        then getImageHandler captures req
                        else throwError (NotFound
                          ("no image index " <> T.pack (show n)
                           <> " of type " <> ty <> " for item " <> i))
            _ -> throwError (BadRequest "invalid image index")

-- | The stored backdrop originals in @dir@: files named @Backdrop@,
-- @Backdrop1@, @Backdrop2@, ... (stem @Backdrop@ + optional digits) with
-- an image extension, excluding resize variants (their base name has a
-- @-@ and never matches). Sorted for stable index selection.
backdropFiles :: FilePath -> IO [FilePath]
backdropFiles dir = do
  dirExists <- doesDirectoryExist dir
  if not dirExists
    then pure []
    else do
      entries <- listDirectory dir
      pure [ dir </> e | e <- sort entries
           , isBackdropName (takeBaseName e)
           , map toLower (takeExtension e) `elem` [".jpg", ".jpeg", ".png", ".webp"] ]
  where
    isBackdropName b = case stripPrefix "Backdrop" b of
      Just rest -> null rest || all isDigit rest
      Nothing   -> False

-- | @POST \/items\/:id\/images\/:type@ — upload raw image bytes (the
-- request body) as the item's image of @type@. The extension comes from
-- the @Content-Type@ header via 'imageExtFor' (default @.jpg@); the
-- file is stored at @stateDir/images/<id>/<type>.<ext>@, replacing any
-- previously stored file of the same type with another extension. 404
-- for an unknown item, 400 for an invalid type, otherwise
-- @{\"status\":\"ok\"}@.
uploadImageHandler :: [(Text, Text)] -> Request -> AppM Response
uploadImageHandler captures req =
  case (lookup "id" captures, lookup "type" captures) of
    (Nothing, _) -> throwError (BadRequest "missing id capture")
    (_, Nothing) -> throwError (BadRequest "missing type capture")
    (Just i, Just ty)
      | not (isValidImageType ty) ->
          throwError (BadRequest ("invalid image type: " <> ty))
      | otherwise -> do
          mItem <- getItemWithAccess req i
          case mItem of
            Nothing -> throwError (NotFound ("no library item with id " <> i))
            Just _ -> do
              cfg  <- asks envConfig
              let stateDir = configStateDir cfg
                  dir      = imagesDirFor stateDir i
              body <- liftIO (strictRequestBody req)
              let mCt  = lookup hContentType (requestHeaders req)
                  ext  = imageExtFor (fromMaybe BS.empty mCt)
                  path = imagePathFor stateDir i ty ext
              liftIO (removeOtherExts dir (T.unpack ty) ext)
              liftIO (createDirectoryIfMissing True (takeDirectory path))
              liftIO (LBS.writeFile path body)
              logInfo ("images: stored " <> ty <> " for item " <> i
                       <> " at " <> T.pack path)
              pure (jsonResponse (object ["status" .= ("ok" :: Text)]))

-- | @DELETE \/items\/:id\/images\/:type@ — remove the stored image of
-- @type@ and all its cached resize variants. 404 for an unknown item or
-- when no such image exists, 400 for an invalid type, otherwise
-- @{\"status\":\"ok\"}@.
deleteImageHandler :: [(Text, Text)] -> Request -> AppM Response
deleteImageHandler captures req =
  case (lookup "id" captures, lookup "type" captures) of
    (Nothing, _) -> throwError (BadRequest "missing id capture")
    (_, Nothing) -> throwError (BadRequest "missing type capture")
    (Just i, Just ty)
      | not (isValidImageType ty) ->
          throwError (BadRequest ("invalid image type: " <> ty))
      | otherwise -> do
          mItem <- getItemWithAccess req i
          case mItem of
            Nothing -> throwError (NotFound ("no library item with id " <> i))
            Just _ -> do
              cfg <- asks envConfig
              let dir = imagesDirFor (configStateDir cfg) i
              targets <- liftIO (imageFilesFor dir (T.unpack ty))
              if null targets
                then throwError (NotFound
                       ("no image of type " <> ty <> " for item " <> i))
                else do
                  liftIO (mapM_ removeFile targets)
                  logInfo ("images: deleted " <> ty <> " for item " <> i
                           <> " (" <> T.pack (show (length targets)) <> " file(s))")
                  pure (jsonResponse (object ["status" .= ("ok" :: Text)]))

-- | Does the item already have a stored image of @type@? @Backdrop@ is
-- special: several backdrop files (@Backdrop@, @Backdrop1@, ...) count as
-- one type. Used by the TMDB artwork fetch to avoid overwriting
-- user-uploaded images.
hasStoredImage :: FilePath -> Text -> Text -> IO Bool
hasStoredImage stateDir i ty = do
  let dir = imagesDirFor stateDir i
  if T.toLower ty == "backdrop"
    then do
      files <- backdropFiles dir
      pure (not (null files))
    else isJust <$> findStoredImage dir ty

-- | Store raw image bytes as the item's image of @ty@ in the same layout
-- as 'uploadImageHandler', returning the path written. @Backdrop@ images
-- go to the next free index stem (@Backdrop@, @Backdrop1@, ...) so several
-- can coexist; other types replace any previously stored file of the same
-- type (other extensions removed, like the upload route). Used by the
-- TMDB artwork fetch (metadata module).
storeImageBytes :: FilePath -> Text -> Text -> String -> BS.ByteString -> IO FilePath
storeImageBytes stateDir i ty ext bytes = do
  let dir = imagesDirFor stateDir i
  stem <- if T.toLower ty == "backdrop"
            then do
              files <- backdropFiles dir
              let n = length files :: Int
              pure (if n == 0 then "Backdrop" else "Backdrop" ++ show n)
            else pure (T.unpack ty)
  if stem == T.unpack ty
    then removeOtherExts dir stem ext
    else pure ()
  let path = dir </> (stem ++ ext)
  createDirectoryIfMissing True (takeDirectory path)
  BS.writeFile path bytes
  pure path

-- | The stored original of @type@ in @dir@ (a file whose base name is
-- exactly the type — resized variants have a @-@ suffix and never
-- match), if any, with its extension.
findStoredImage :: FilePath -> Text -> IO (Maybe (FilePath, String))
findStoredImage dir ty = do
  dirExists <- doesDirectoryExist dir
  if not dirExists
    then pure Nothing
    else do
      entries <- listDirectory dir
      pure $ case [ e | e <- sort entries, takeBaseName e == T.unpack ty ] of
        (e : _) -> Just (dir </> e, takeExtension e)
        []      -> Nothing

-- | All files belonging to image @stem@ in @dir@: the original
-- (@<stem>.<ext>@) plus every cached resize variant (@<stem>-<w>x<h>.jpg@).
imageFilesFor :: FilePath -> String -> IO [FilePath]
imageFilesFor dir stem = do
  dirExists <- doesDirectoryExist dir
  if not dirExists
    then pure []
    else do
      entries <- listDirectory dir
      pure [ dir </> e | e <- sort entries, let b = takeBaseName e
           , b == stem || (stem ++ "-") `isPrefixOf` b ]

-- | Remove stored originals of @stem@ whose extension differs from
-- @keepExt@, so an upload leaves exactly one original per type.
removeOtherExts :: FilePath -> String -> String -> IO ()
removeOtherExts dir stem keepExt = do
  dirExists <- doesDirectoryExist dir
  when dirExists $ do
    entries <- listDirectory dir
    mapM_ (removeFile . (dir </>))
      [ e | e <- entries, takeBaseName e == stem, takeExtension e /= keepExt ]

-- | Serve one stored image file @path@ (extension @ext@, type @ty@,
-- item @i@): ETag (the file's 'imageTagOfFile') + @Cache-Control:
-- public, max-age=86400@ headers (milestone 6g), and the usual optional
-- ffmpeg resize into the cached variant when @maxWidth@\/@maxHeight@ is
-- present.
serveStoredImage :: FilePath -> Text -> Text -> FilePath -> String -> Request -> AppM Response
serveStoredImage dir i ty path ext req = do
  tag <- liftIO (imageTagOfFile path)
  let qp   = queryParams (queryString req)
      -- hardening: numeric resize params are clamped (width/height to
      -- [1, 8192], quality to [1, 100]) so no value can cause absurd work
      mW   = lookup "maxwidth"  qp >>= parseBoundedInt 1 8192 1
      mH   = lookup "maxheight" qp >>= parseBoundedInt 1 8192 1
      q    = fromMaybe defaultQuality (lookup "quality" qp >>= parseBoundedInt 1 100 defaultQuality)
      hdrs = [(mk "ETag", tagB tag), (mk "Cache-Control", "public, max-age=86400")]
  if isNothing mW && isNothing mH
    then serveImageFileWithHeaders path (contentTypeForExt ext) hdrs
    else do
      let vPath = variantPathFor dir ty mW mH
      vExists <- liftIO (doesFileExist vPath)
      unless vExists $ do
        liftIO (createDirectoryIfMissing True dir)
        logInfo ("images: resizing " <> ty <> " for item " <> i)
        (ec, _out, err) <- liftIO
          (runProcessBS "ffmpeg" (resizeArgs path mW mH q vPath))
        case ec of
          ExitSuccess -> pure ()
          ExitFailure n ->
            throwError (InternalError (T.pack
              ("ffmpeg image resize failed (exit " ++ show n ++ "): "
               ++ take 2000 (showBS (LBS.toStrict err)))))
      serveImageFileWithHeaders vPath "image/jpeg" hdrs

-- | A 'Text' tag as an HTTP header value (ASCII: digits and dash).
tagB :: Text -> BS.ByteString
tagB = BSC.pack . T.unpack

-- | Read a whole image file and serve it with 'responseLBS', the given
-- Content-Type and extra headers.
serveImageFileWithHeaders :: FilePath -> BS.ByteString -> ResponseHeaders -> AppM Response
serveImageFileWithHeaders path contentType extra = do
  bs <- liftIO (LBS.readFile path)
  pure (responseLBS status200 (("Content-Type", contentType) : extra) bs)

-- Blurhash placeholders (Jellyfin BaseItemDto.ImageBlurHashes parity).
-- Pure encoder lives in "FMS.Core.Blurhash"; here we extract 32x32
-- RGB24 pixels with ffmpeg and cache the computed hash per image file
-- in @stateDir/blurhashes.json@, keyed by absolute path and invalidated
-- by the existing image tag (size-mtime).

-- | Registry key for the in-memory blurhash cache TVar.
blurhashCacheKey :: Text
blurhashCacheKey = "images.blurhash"

-- | One cache entry: the image tag the hash was computed for, and the
-- hash itself.
data BlurhashEntry = BlurhashEntry
  { bhTag  :: !Text
  , bhHash :: !Text
  } deriving (Eq, Show)

instance ToJSON BlurhashEntry where
  toJSON e = object ["tag" .= bhTag e, "hash" .= bhHash e]

instance FromJSON BlurhashEntry where
  parseJSON = withObject "BlurhashEntry" $ \o ->
    BlurhashEntry <$> o .: "tag" <*> o .: "hash"

blurhashCacheFile :: Config -> FilePath
blurhashCacheFile cfg = configStateDir cfg </> "blurhashes.json"

-- | Strictly load the persisted blurhash cache; a missing or malformed
-- file yields an empty map.
loadBlurhashCache :: Config -> IO (HashMap FilePath BlurhashEntry)
loadBlurhashCache cfg = do
  let path = blurhashCacheFile cfg
  exists <- doesFileExist path
  if not exists
    then pure HM.empty
    else do
      raw <- BS.readFile path
      pure (fromMaybe HM.empty (Aeson.decode (LBS.fromStrict raw)))

-- | Persist the cache (strict write).
saveBlurhashCache :: Config -> HashMap FilePath BlurhashEntry -> IO ()
saveBlurhashCache cfg m =
  BS.writeFile (blurhashCacheFile cfg) (LBS.toStrict (Aeson.encode m))

-- | The blurhash cache TVar, created lazily (the ensureUserDataTVar
-- pattern); the persisted JSON is loaded on first use.
ensureBlurhashTVar :: AppM (TVar (HashMap FilePath BlurhashEntry))
ensureBlurhashTVar = do
  mTv <- getState blurhashCacheKey
  case mTv of
    Just tv -> pure tv
    Nothing -> do
      cfg <- asks envConfig
      m <- liftIO (loadBlurhashCache cfg)
      tv <- liftIO (newTVarIO m)
      putState blurhashCacheKey tv
      pure tv

-- | Extract 32x32 RGB24 pixels from an image file with ffmpeg
-- (@-f rawvideo -pix_fmt rgb24 pipe:1@, captured as raw bytes via
-- "FMS.Core.Process"). 'Nothing' when ffmpeg fails or the output is
-- not exactly 32*32*3 bytes.
blurhashPixels :: FilePath -> IO (Maybe [Word8])
blurhashPixels path = do
  (ec, out, _err) <- runProcessBSTimeout 15 "ffmpeg"
    [ "-hide_banner", "-loglevel", "error", "-i", path
    , "-vf", "scale=32:32", "-f", "rawvideo", "-pix_fmt", "rgb24"
    , "pipe:1"
    ]
  case ec of
    ExitSuccess | LBS.length out == 32 * 32 * 3 ->
      pure (Just (BS.unpack (LBS.toStrict out)))
    _ -> pure Nothing

-- | The blurhash of an image file (4x3 components), or 'Nothing' when
-- the pixels cannot be extracted.
blurhashForImageFile :: FilePath -> IO (Maybe String)
blurhashForImageFile path = do
  mPx <- blurhashPixels path
  pure (encodeBlurhash 32 32 4 3 <$> mPx)

-- | The blurhash of a stored item image, using the cache: when the
-- cached entry's tag matches the file's current 'imageTagOfFile' the
-- cached hash is returned; otherwise the hash is recomputed, cached
-- and persisted. 'Nothing' (and no caching) when ffmpeg cannot read
-- the file.
blurhashForStoredImage :: Config -> FilePath -> AppM (Maybe Text)
blurhashForStoredImage cfg path = do
  tag <- liftIO (imageTagOfFile path)
  tv  <- ensureBlurhashTVar
  mCached <- liftIO (HM.lookup path <$> readTVarIO tv)
  case mCached of
    Just e | bhTag e == tag -> pure (Just (bhHash e))
    _ -> do
      mHash <- liftIO (blurhashForImageFile path)
      case mHash of
        Nothing -> pure Nothing
        Just h  -> do
          let hT = T.pack h
              entry = BlurhashEntry tag hT
          liftIO $ atomically $ modifyTVar' tv (HM.insert path entry)
          m <- liftIO (readTVarIO tv)
          liftIO (saveBlurhashCache cfg m)
          pure (Just hT)

-- | The @(imageType, blurhash)@ pairs for every stored image of an
-- item (@stateDir/images/<itemId>@); types whose hash could not be
-- computed are skipped.
blurhashPairsForItem :: Config -> Text -> AppM [(Text, Text)]
blurhashPairsForItem cfg i = do
  let stateDir = configStateDir cfg
      dir = imagesDirFor stateDir i
  types <- liftIO (listImageTypes stateDir i)
  pairs <- forM types $ \ty -> do
    mF <- liftIO (findStoredImage dir ty)
    case mF of
      Just (fp, _) -> do
        mh <- blurhashForStoredImage cfg fp
        pure (ty, mh)
      Nothing -> pure (ty, Nothing)
  pure [ (ty, h) | (ty, Just h) <- pairs ]
