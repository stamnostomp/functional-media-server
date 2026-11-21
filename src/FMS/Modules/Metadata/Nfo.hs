module FMS.Modules.Metadata.Nfo
  ( parseMovieNfo
  , parseTvShowNfo
  , parseEpisodeNfo
  , EpisodeNfo(..)
  , episodeNfoXml
  , tvShowNfoXml
  , movieNfoXml
  , xmlEscape
  , nfoPathFor
  ) where

import Control.Applicative ((<|>))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
import Data.List (nub)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath ((<.>), dropExtension)
import Text.Read (readMaybe)
import Text.XML (def, documentRoot, elementName, nameLocalName, parseLBS)
import Text.XML.Cursor (Cursor, child, content, fromDocument, laxElement)

import FMS.Modules.Metadata.Types (MediaMetadata (..))

-- | The NFO sidecar for a media file: same path with the extension
-- replaced by @.nfo@ (Kodi/Jellyfin convention: @movie.mp4@ ->
-- @movie.nfo@).
nfoPathFor :: FilePath -> FilePath
nfoPathFor path = dropExtension path <.> "nfo"

-- | Parse a Kodi/Jellyfin-style @<movie>...</movie>@ NFO file into a
-- 'MediaMetadata'. Pure: malformed XML (or a document whose root is not
-- @<movie>@) is a 'Left'; fields the file does not carry simply stay
-- 'Nothing' / empty. 'metaSource' is always @"nfo"@.
parseMovieNfo :: BS.ByteString -> Either String MediaMetadata
parseMovieNfo bytes =
  case parseLBS def (LBS.fromStrict bytes) of
    Left err -> Left ("malformed XML: " ++ show err)
    Right doc
      -- 'fromDocument' points at the root element itself, so check the
      -- root's name directly (case-insensitively, like 'laxElement').
      | T.toCaseFold (nameLocalName (elementName (documentRoot doc))) == "movie" ->
          Right (movieFromCursor (fromDocument doc))
      | otherwise -> Left "no <movie> root element"

-- | Build the 'MediaMetadata' from a @<movie>@ element cursor.
movieFromCursor :: Cursor -> MediaMetadata
movieFromCursor m =
  MediaMetadata
    { metaTitle          = firstText "title" m
    , metaOverview       = firstText "overview" m
    , metaGenres         = allTexts "genre" m
    , metaStudios        = allTexts "studio" m          -- milestone 4h
    , metaPersons        = personNames m                -- milestone 4h
    , metaTags           = allTexts "tag" m             -- milestone 4h
    , metaYear           = readIntField "year" m
    , metaRating         = readDoubleField "rating" m
    , metaOfficialRating = firstText "mpaa" m           -- milestone 9
    , metaRuntimeMinutes = readIntField "runtime" m
    , metaProviderIds    = HM.fromList (providerIds m)
    , metaSource         = "nfo"
    , metaKeywords       = []                          -- m53a (NFO carries no keywords)
    , metaLockedFields   = []                          -- milestone 4k
    }

-- | Text of all child elements with the given local name, stripped, with
-- empty/whitespace-only entries dropped (e.g. all @<genre>@ values).
-- ('laxElement' filters the node it is applied to, so go through 'child'
-- first: @child c >>= laxElement name@ selects c's child elements.)
allTexts :: Text -> Cursor -> [Text]
allTexts name c =
  [ T.strip t
  | e <- child c >>= laxElement name
  , t <- child e >>= content
  , not (T.null (T.strip t))
  ]

-- | Text of the first non-empty child element with the given local name.
firstText :: Text -> Cursor -> Maybe Text
firstText name c = listToMaybe (allTexts name c)

-- | Milestone 4h: text of the @childName@ sub-elements of every @parent@
-- child element, e.g. all @<name>@ values inside @<actor>@ elements.
allChildTexts :: Text -> Text -> Cursor -> [Text]
allChildTexts parent childName c =
  [ T.strip t
  | e <- child c >>= laxElement parent
  , n <- child e >>= laxElement childName
  , t <- child n >>= content
  , not (T.null (T.strip t))
  ]

-- | Milestone 4h: all person names of an NFO element — @<actor><name>..@
-- and @<person><name>..@ (nested) plus plain @<director>..</director>@
-- text (Kodi NFO conventions), deduplicated, order preserved.
personNames :: Cursor -> [Text]
personNames c =
  nub (  allChildTexts "actor" "name" c
      ++ allTexts "director" c
      ++ allChildTexts "person" "name" c
      )

-- | Read a child element's text as an 'Int' (@<year>2020</year>@,
-- @<runtime>95</runtime>@); unparseable text is 'Nothing'.
readIntField :: Text -> Cursor -> Maybe Int
readIntField name c = firstText name c >>= \t -> readMaybe (T.unpack t)

-- | Read a child element's text as a 'Double' (@<rating>7.5</rating>@).
readDoubleField :: Text -> Cursor -> Maybe Double
readDoubleField name c = firstText name c >>= \t -> readMaybe (T.unpack t)

-- | Provider ids from the standard Kodi NFO tags: @<tmdbid>42</tmdbid>@ ->
-- @("tmdb", "42")@, @<imdbid>tt0133093</imdbid>@ -> @("imdb", "tt0133093")@.
providerIds :: Cursor -> [(Text, Text)]
providerIds m =
  [ (key, v)
  | (tag, key) <- [("tmdbid", "tmdb"), ("imdbid", "imdb")]
  , Just v <- [firstText tag m]
  ]

-- | Parse a Kodi/Jellyfin-style <tvshow>...</tvshow> NFO (the series-level
-- sidecar that lives next to the episode files, milestone 4d) into a
-- 'MediaMetadata'. Same rules as 'parseMovieNfo': malformed XML or a
-- non-<tvshow> root is a 'Left'; <premiered>YYYY-MM-DD</premiered> is read
-- as the year (first 4 chars), <rating> as a double, <tmdbid>/<imdbid>
-- into 'metaProviderIds'. 'metaSource' is always "nfo".
parseTvShowNfo :: BS.ByteString -> Either String MediaMetadata
parseTvShowNfo bytes =
  case parseLBS def (LBS.fromStrict bytes) of
    Left err -> Left ("malformed XML: " ++ show err)
    Right doc
      | T.toCaseFold (nameLocalName (elementName (documentRoot doc))) == "tvshow" ->
          Right (tvShowFromCursor (fromDocument doc))
      | otherwise -> Left "no <tvshow> root element"

-- | Build the 'MediaMetadata' from a <tvshow> element cursor.
tvShowFromCursor :: Cursor -> MediaMetadata
tvShowFromCursor c =
  MediaMetadata
    { metaTitle          = firstText "title" c
    , metaOverview       = firstText "overview" c
    , metaGenres         = allTexts "genre" c
    , metaStudios        = allTexts "studio" c          -- milestone 4h
    , metaPersons        = personNames c                -- milestone 4h
    , metaTags           = allTexts "tag" c             -- milestone 4h
    , metaYear           = readIntField "year" c <|> premieredYear c
    , metaRating         = readDoubleField "rating" c
    , metaOfficialRating = firstText "mpaa" c           -- milestone 9
    , metaRuntimeMinutes = Nothing
    , metaProviderIds    = HM.fromList (providerIds c)
    , metaSource         = "nfo"
    , metaKeywords       = []                          -- m53a (NFO carries no keywords)
    , metaLockedFields   = []                          -- milestone 4k
    }

-- | Year from <premiered>2022-01-01</premiered> (first 4 chars).
premieredYear :: Cursor -> Maybe Int
premieredYear c = firstText "premiered" c >>= \t -> readMaybe (take 4 (T.unpack (T.strip t)))


-- | m76: the parsed content of a Kodi per-episode @<episodedetails>..@
-- sidecar (@<file>.nfo@ next to an episode media file).
data EpisodeNfo = EpisodeNfo
  { enTitle   :: Maybe Text
  , enSeason  :: Maybe Int
  , enEpisode :: Maybe Int
  , enTmdbId  :: Maybe Text   -- ^ <tmdbid> (series id) or <uniqueid type="tmdb">
  } deriving (Eq, Show)

-- | Parse a per-episode @<episodedetails>@ NFO. Malformed XML or a
-- different root is a 'Left'.
parseEpisodeNfo :: BS.ByteString -> Either String EpisodeNfo
parseEpisodeNfo bytes =
  case parseLBS def (LBS.fromStrict bytes) of
    Left err -> Left ("malformed XML: " ++ show err)
    Right doc
      | T.toCaseFold (nameLocalName (elementName (documentRoot doc))) == "episodedetails" ->
          Right (episodeFromCursor (fromDocument doc))
      | otherwise -> Left "no <episodedetails> root element"

episodeFromCursor :: Cursor -> EpisodeNfo
episodeFromCursor c =
  EpisodeNfo
    { enTitle   = firstText "title" c
    , enSeason  = readIntField "season" c
    , enEpisode = readIntField "episode" c
    , enTmdbId  = firstText "tmdbid" c <|> firstText "uniqueid" c
    }

-- | Escape the five XML special characters (for NFO text fields).
xmlEscape :: Text -> Text
xmlEscape = T.concatMap esc
  where
    esc '&' = "&amp;"
    esc '<' = "&lt;"
    esc '>' = "&gt;"
    esc '"' = "&quot;"
    esc '\'' = "&apos;"
    esc c   = T.singleton c

-- | m76: a Kodi per-episode NFO body: title, season, episode and the
-- TMDB series id (both <tmdbid> and <uniqueid type="tmdb">). Pure.
episodeNfoXml :: Text -> Int -> Int -> Int -> Text
episodeNfoXml title season episode tmdbId =
  T.concat
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<episodedetails>\n"
    , "  <title>", xmlEscape title, "</title>\n"
    , "  <season>", T.pack (show season), "</season>\n"
    , "  <episode>", T.pack (show episode), "</episode>\n"
    , "  <tmdbid>", T.pack (show tmdbId), "</tmdbid>\n"
    , "  <uniqueid type=\"tmdb\" default=\"true\">", T.pack (show tmdbId), "</uniqueid>\n"
    , "</episodedetails>\n" ]

-- | m76: a series-level @tvshow.nfo@ body (title + TMDB id). Pure.
tvShowNfoXml :: Text -> Int -> Text
tvShowNfoXml title tmdbId =
  T.concat
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<tvshow>\n"
    , "  <title>", xmlEscape title, "</title>\n"
    , "  <tmdbid>", T.pack (show tmdbId), "</tmdbid>\n"
    , "  <uniqueid type=\"tmdb\" default=\"true\">", T.pack (show tmdbId), "</uniqueid>\n"
    , "</tvshow>\n" ]

-- | m76: a movie NFO body (title + TMDB id + optional year). Pure.
movieNfoXml :: Text -> Int -> Maybe Int -> Text
movieNfoXml title tmdbId mYear =
  T.concat
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<movie>\n"
    , "  <title>", xmlEscape title, "</title>\n"
    , maybe "" (\y -> "  <year>" <> T.pack (show y) <> "</year>\n") mYear
    , "  <tmdbid>", T.pack (show tmdbId), "</tmdbid>\n"
    , "  <uniqueid type=\"tmdb\" default=\"true\">", T.pack (show tmdbId), "</uniqueid>\n"
    , "</movie>\n" ]
