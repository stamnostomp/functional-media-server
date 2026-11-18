module FMS.Modules.Metadata.Api.Types
  ( TmdbResult(..)
  , releaseYear
  , cleanTitleForProvider
  , pickTmdbResult
  ) where

import Data.Aeson (FromJSON (..), withObject, (.:), (.:?))
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

-- | One movie hit from the TMDB @\/search\/movie@ endpoint (milestone 4c,
-- docs/roadmap.org milestone 4). Only the fields the server stores in
-- 'FMS.Modules.Metadata.Types.MediaMetadata' are decoded. 'tmdbReleaseYear'
-- is extracted from the @release_date@ string (@\"YYYY-MM-DD\"@); missing
-- or malformed dates become 'Nothing' (TMDB uses @\"\"@ for unknown).
data TmdbResult = TmdbResult
  { tmdbId          :: Int          -- ^ TMDB's own movie id ("id")
  , tmdbTitle       :: Text         -- ^ "title" (movies) / "name" (tv, mapped)
  , tmdbOverview    :: Maybe Text   -- ^ "overview" (often present, but optional)
  , tmdbReleaseYear :: Maybe Int    -- ^ year of "release_date"/"first_air_date" ("YYYY-MM-DD")
  , tmdbVoteAverage :: Maybe Double -- ^ "vote_average" (community rating 0-10)
  , tmdbPoster      :: Maybe Text   -- ^ "poster_path" (m26; both movie and tv payloads carry it)
  } deriving (Show, Eq)

instance FromJSON TmdbResult where
  parseJSON = withObject "TmdbResult" $ \o -> do
    rid      <- o .:  "id"
    title    <- o .:  "title"
    overview <- o .:? "overview"
    mDate    <- o .:? "release_date"
    vote     <- o .:? "vote_average"
    poster   <- o .:? "poster_path"
    pure TmdbResult
      { tmdbId          = rid
      , tmdbTitle       = title
      , tmdbOverview    = overview
      , tmdbReleaseYear = mDate >>= releaseYear
      , tmdbVoteAverage = vote
      , tmdbPoster      = poster
      }

-- | Extract the year from a TMDB @release_date@ (@\"YYYY-MM-DD\"@): take the
-- part before the first dash and read it as an 'Int'. Empty/malformed
-- strings are 'Nothing'.
releaseYear :: Text -> Maybe Int
releaseYear date =
  case T.splitOn "-" date of
    (y : _) -> readMaybe (T.unpack y)
    []      -> Nothing

-- | Release-tag tokens (lowercase) dropped by 'cleanTitleForProvider':
-- resolutions, codecs, sources, audio tags and misc scene tags.
releaseTagTokens :: [Text]
releaseTagTokens =
  [ "720p", "1080p", "2160p", "4k", "8k", "480p", "576p"
  , "x264", "x265", "h264", "h265", "hevc", "av1", "avc", "vp9"
  , "bluray", "blu-ray", "bdrip", "bdremux", "remux", "webrip", "web-dl"
  , "web", "dl", "hdtv", "dvdrip", "dvd", "cam", "ts", "m2ts", "pdtv"
  , "uhd", "hdrip"
  , "aac", "dts", "ddp", "dd", "ac3", "atmos", "truehd", "mp3", "flac"
  , "hdr", "hdr10", "hdr10plus", "dv", "dolbyvision", "proper", "repack"
  , "extended", "unrated", "imax", "anime", "remastered"
  ]

-- | Clean a scene/release-style file title for a provider (TMDB) search:
-- replace dots/underscores/dashes with spaces, tokenize, drop release-tag
-- tokens, bracketed groups and a trailing release-group token, extract the
-- first 4-digit 1900-2099 token as the year (removed from the query), then
-- collapse whitespace. Returns the cleaned query and the optional year.
-- Examples: @Shrek.2001.1080p.BluRay.x265-LAMA@ -> (@"Shrek"@, @Just 2001@);
-- @The.Matrix.1999.2160p.UHD.BluRay.REMUX-FraMeSToR@ -> (@"The Matrix"@,
-- @Just 1999@); @sample-long@ -> (@"sample long"@, 'Nothing').
cleanTitleForProvider :: Text -> (Text, Maybe Int)
cleanTitleForProvider raw =
  let spaced = T.map (\c -> if c == '.' || c == '_' || c == '-' then ' ' else c) raw
      toks0 = filter (not . T.null) (T.words spaced)
      isBracketed t = T.any (\c -> c == '(' || c == ')' || c == '[' || c == ']'
                                   || c == '{' || c == '}') t
      isTag t = T.toLower t `elem` releaseTagTokens
      toks1 = filter (\t -> not (isBracketed t) && not (isTag t)) toks0
      isYearTok t = case T.unpack t of
        [a, b, c, d] -> all (`elem` ("0123456789" :: String)) [a, b, c, d]
                        && let y = read (T.unpack t) :: Int in y >= 1900 && y <= 2099
        _ -> False
      (mYear, toks2) = case break isYearTok toks1 of
        (before, yt : after) -> (Just (read (T.unpack yt) :: Int), before ++ after)
        (_, [])              -> (Nothing, toks1)
      -- a trailing release-group token: all-caps (LAMA) or camel-mixed
      -- (FraMeSToR); a plain capitalized word (Club) is kept
      isGroupTok t =
        let letters = T.unpack (T.filter (\c -> c /= '\'' && c /= '&') t)
        in length letters >= 2
           && (all (\c -> not (c >= 'a' && c <= 'z')) letters
               || any (\c -> c >= 'A' && c <= 'Z') (drop 1 letters))
      toks3 = case reverse toks2 of
        (t : rest) | isGroupTok t -> reverse rest
        _                        -> toks2
      clean = T.unwords toks3
      clean' = if T.null clean then T.unwords toks0 else clean
  in (clean', mYear)

-- | Pick the best TMDB hit for an extracted year: when a year was found,
-- prefer the first result whose release year equals it, otherwise the first
-- result. 'Nothing' on an empty list (caller treats that as no results).
pickTmdbResult :: Maybe Int -> [TmdbResult] -> Maybe TmdbResult
pickTmdbResult _ [] = Nothing
pickTmdbResult mYear rs =
  case mYear of
    Nothing -> Just (head rs)
    Just y  -> case [ r | r <- rs, tmdbReleaseYear r == Just y ] of
      (r : _) -> Just r
      []      -> Just (head rs)
