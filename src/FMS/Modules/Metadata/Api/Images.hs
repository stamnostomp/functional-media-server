module FMS.Modules.Metadata.Api.Images
  ( tmdbImagesUrl
  , tmdbImageListToDownloads
  , pickByLanguage
  , TmdbImageEntry (..)
  , fetchTmdbImages
  ) where

import Control.Exception (SomeException, try)
import qualified Data.Aeson as Aeson
import Data.Aeson (FromJSON (..), Result (..), Value, fromJSON, withObject, (.:?), (.!=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T

import FMS.Modules.Metadata.Api.Tmdb (tmdbFetch)

-- | TMDB artwork download (Jellyfin parity: posters/backdrops appear
-- without manual upload). Opt-in via FMS_FETCH_IMAGES; the API key is the
-- same MEDIA_TMDB_API_KEY as the metadata search, the endpoints/base URLs
-- are configurable (FMS_TMDB_BASE_URL / FMS_TMDB_IMAGE_BASE_URL /
-- FMS_TMDB_IMAGE_SIZE) so tests can point them at a mock server.

-- | Build the TMDB @/{kind}/{id}/images@ URL (kind = @movie@ or @tv@).
-- Pure, so it is unit-testable. A trailing slash on the base is stripped.
tmdbImagesUrl :: Text -> Text -> Text -> Text -> String
tmdbImagesUrl apiBase apiKey kind tmdbId =
  stripSlash apiBase ++ "/" ++ T.unpack kind ++ "/" ++ T.unpack tmdbId
    ++ "/images?api_key=" ++ T.unpack apiKey
  where
    stripSlash t = T.unpack (T.dropWhileEnd (== '/') t)

-- | One entry of a TMDB @/images@ list: @file_path@ plus the m39
-- @iso_639_1@ language tag (used to prefer English artwork).
data TmdbImageEntry = TmdbImageEntry
  { tieFilePath :: Maybe Text
  , tieLang     :: Maybe Text
  }

instance FromJSON TmdbImageEntry where
  parseJSON = withObject "TmdbImageEntry" $ \o ->
    TmdbImageEntry
      <$> o .:? "file_path"
      <*> o .:? "iso_639_1"

-- | m39: stable reorder putting English (lang @en@) and language-less
-- entries FIRST, then everything else in original order.
pickByLanguage :: [TmdbImageEntry] -> [TmdbImageEntry]
pickByLanguage es = filter isPreferred es ++ filter (not . isPreferred) es
  where
    isPreferred e = case tieLang e of
      Just l  -> T.toLower l == "en"
      Nothing -> True

-- | The TMDB @/images@ document: @posters@ / @backdrops@ / @logos@.
data TmdbImagesDoc = TmdbImagesDoc
  { tidPosters   :: [TmdbImageEntry]
  , tidBackdrops :: [TmdbImageEntry]
  , tidLogos     :: [TmdbImageEntry]
  }

instance FromJSON TmdbImagesDoc where
  parseJSON = withObject "TmdbImagesDoc" $ \o ->
    TmdbImagesDoc
      <$> o .:? "posters"   .!= []
      <*> o .:? "backdrops" .!= []
      <*> o .:? "logos"     .!= []

-- | Pure: map the TMDB @/images@ JSON (@{\"posters\":[...],
-- \"backdrops\":[...], \"logos\":[...]@}) to @(imageType, file_path)@
-- download pairs: posters -> @Primary@ (cap 1), backdrops -> @Backdrop@
-- (cap 3), logos -> @Logo@ (cap 1); entries with a missing or empty
-- @file_path@ are skipped; a non-object value yields [].
tmdbImageListToDownloads :: Value -> [(Text, Text)]
tmdbImageListToDownloads v = case fromJSON v :: Result TmdbImagesDoc of
  Success doc ->
       take 1 [ ("Primary", p) | Just p <- map tieFilePath (pickByLanguage (tidPosters doc)), not (T.null p) ]
    ++ take 3 [ ("Backdrop", p) | Just p <- map tieFilePath (pickByLanguage (tidBackdrops doc)), not (T.null p) ]
    ++ take 1 [ ("Logo", p) | Just p <- map tieFilePath (pickByLanguage (tidLogos doc)), not (T.null p) ]
  _ -> []

-- | Fetch the TMDB image list for @kind@ \/ @tmdbId@ and download each
-- file from @{imageBase}/{imageSize}{file_path}@ (10 s timeout each via
-- 'tmdbFetch'). Returns the successfully downloaded
-- @(imageType, file_path, bytes)@ triples (possibly empty). Never throws:
-- transport errors, non-200s and malformed JSON all come back as 'Left'
-- or an empty/partial list, so a scan is never crashed by artwork.
fetchTmdbImages
  :: Text                    -- ^ TMDB API key
  -> Text                    -- ^ API base URL (configTmdbBaseUrl)
  -> Text                    -- ^ @movie@ or @tv@
  -> Text                    -- ^ TMDB id
  -> Text                    -- ^ image CDN base (configTmdbImageBaseUrl)
  -> Text                    -- ^ poster image size (configTmdbImageSize, e.g. w500)
  -> Text                    -- ^ backdrop/logo image size (m49: configTmdbBackdropSize, e.g. w1280)
  -> IO (Either String [(Text, Text, BS.ByteString)])
fetchTmdbImages apiKey apiBase kind tmdbId imageBase imageSize backdropSize = do
  eres <- tmdbFetch (tmdbImagesUrl apiBase apiKey kind tmdbId)
  case eres of
    Left err -> pure (Left err)
    Right body ->
      case Aeson.decode body of
        Nothing -> pure (Left "tmdb images: malformed JSON body")
        Just v  -> do
          let dl = tmdbImageListToDownloads v
              -- m49: backdrops/logos download at the (higher-res) backdrop
              -- size, posters at the normal image size
              sizeFor ty = if ty == "Primary" then imageSize else backdropSize
              imgUrl ty fp = T.unpack (T.dropWhileEnd (== '/') imageBase)
                          ++ "/" ++ T.unpack (sizeFor ty) ++ T.unpack fp
          pairs <- mapM (\(ty, fp) -> do
                    r <- try (tmdbFetch (imgUrl ty fp))
                           :: IO (Either SomeException (Either String LBS.ByteString))
                    pure (fmap (fmap (\b -> (ty, fp, LBS.toStrict b))) r))
                dl
          -- keep only the downloads that succeeded
          pure (Right [ t | Right (Right t) <- pairs ])
