module FMS.Modules.Metadata.Nfo
  ( parseMovieNfo
  , nfoPathFor
  ) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HM
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
    , metaYear           = readIntField "year" m
    , metaRating         = readDoubleField "rating" m
    , metaRuntimeMinutes = readIntField "runtime" m
    , metaProviderIds    = HM.fromList (providerIds m)
    , metaSource         = "nfo"
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
