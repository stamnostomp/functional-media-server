module FMS.Core.SceneDetect
  ( parseBlackdetect
  , parseSilencedetect
  , mergeIntervals
  , proposeSegments
  , intervalsOverlap
  , blackdetectArgs
  , silencedetectArgs
  ) where

import Data.Char (isDigit)
import Data.List (sortOn)
import Data.Maybe (mapMaybe)

-- Pure parsers for ffmpeg scene/silence detection filter output plus the
-- intro/outro proposal heuristic (Jellyfin MediaSegments AutoDetect
-- parity). No package dependencies beyond base.

-- | Read the first floating-point number that appears after @key@ in a
-- line, e.g. @extract "black_start:" "[blackdetect @ 0x1] black_start:0 black_end:3.04 ..."@
-- is @Just 0@.
extractNumber :: String -> String -> Maybe Double
extractNumber key l = case dropSubstr key l of
  Just rest -> case reads (dropWhile (not . isNumChar) rest) of
    (v, _) : _ -> Just v
    []         -> Nothing
  Nothing -> Nothing
  where
    isNumChar c = isDigit c || c `elem` (".+-eE" :: String)
    dropSubstr k s = case breakOnStr k s of
      Just (_, rest) -> Just rest
      Nothing        -> Nothing
    breakOnStr k s
      | null k = Just (s, "")
      | take (length k) s == k = Just (s, drop (length k) s)
      | otherwise = case s of
          []       -> Nothing
          (c : cs) -> case breakOnStr k cs of
            Just (pre, rest) -> Just (c : pre, rest)
            Nothing          -> Nothing

-- | Parse ffmpeg @blackdetect@ stderr lines into @(start, end)@ second
-- intervals. Lines look like:
-- @[blackdetect @ 0x...] black_start:0 black_end:3.04 black_duration:3.04@.
-- When @black_end@ is missing, @black_duration@ is used instead.
-- Unrelated lines are ignored.
parseBlackdetect :: String -> [(Double, Double)]
parseBlackdetect = mapMaybe lineTo . lines
  where
    lineTo l =
      case (extractNumber "black_start:" l, extractNumber "black_end:" l, extractNumber "black_duration:" l) of
        (Just s, Just e, _)       -> Just (s, e)
        (Just s, Nothing, Just d) -> Just (s, s + d)
        _                         -> Nothing

-- | Parse ffmpeg @silencedetect@ stderr lines into @(start, end)@
-- intervals. @silence_start@ and @silence_end@ come on separate lines
-- and are paired in order; a trailing unpaired @silence_start@ (stream
-- ends in silence) is dropped. When @silence_end@ is missing,
-- @silence_duration@ is used.
parseSilencedetect :: String -> [(Double, Double)]
parseSilencedetect = go Nothing . lines
  where
    go _ [] = []
    go pending (l : rest) =
      case (extractNumber "silence_start:" l, extractNumber "silence_end:" l, extractNumber "silence_duration:" l) of
        (Nothing, Just e, _) ->
          case pending of
            Just s  -> (s, e) : go Nothing rest
            Nothing -> go Nothing rest
        (Nothing, Nothing, Just d) ->
          case pending of
            Just s  -> (s, s + d) : go Nothing rest
            Nothing -> go Nothing rest
        (Just s, Nothing, Just d) -> (s, s + d) : go Nothing rest
        (Just s, _, _) ->
          -- a start while one is pending: the previous one never ended -> drop it
          go (Just s) rest
        _ -> go pending rest

-- | Merge overlapping or adjacent (gap < 0.5 s) intervals; the result
-- is sorted by start.
mergeIntervals :: [(Double, Double)] -> [(Double, Double)]
mergeIntervals ivs = go (sortOn fst ivs)
  where
    go [] = []
    go ((s, e) : rest) = mergeInto (s, e) rest
    mergeInto (s, e) [] = [(s, e)]
    mergeInto (s, e) ((s2, e2) : rest)
      | s2 <= e + 0.5 = mergeInto (s, max e e2) rest
      | otherwise     = (s, e) : mergeInto (s2, e2) rest

-- | Do two @(start, end)@ intervals overlap?
intervalsOverlap :: (Double, Double) -> (Double, Double) -> Bool
intervalsOverlap (a, b) (c, d) = a < d && c < b

-- | The ffmpeg argument list for the black-frame detection pass. The
-- filter logs go to stderr, which the caller parses with
-- 'parseBlackdetect'. Pure so it is unit-testable.
blackdetectArgs :: FilePath -> [String]
blackdetectArgs path =
  [ "-hide_banner", "-nostats"
  , "-i", path
  , "-vf", "blackdetect=d=0.05:pix_th=0.10"
  , "-an", "-f", "null", "-"
  ]

-- | The ffmpeg argument list for the silence detection pass; stderr is
-- parsed with 'parseSilencedetect'.
silencedetectArgs :: FilePath -> [String]
silencedetectArgs path =
  [ "-hide_banner", "-nostats"
  , "-i", path
  , "-af", "silencedetect=n=-35dB:d=0.4"
  , "-f", "null", "-"
  ]

-- | Propose an intro and outro from black + silence intervals.
--
-- @proposeSegments duration blackIntervals silenceIntervals@ returns
-- @(intro, outro)@ as @(start, end)@ seconds. The intro is the maximal
-- chain of merged intervals starting at t = 0 (within a 2 s tolerance)
-- and extending until the first gap >= 0.5 s, capped at 300 s. The
-- outro is the mirror image at the end of the media (within 2 s of
-- @duration@), also capped at 300 s. 'Nothing' when there is no
-- qualifying interval.
proposeSegments
  :: Double                -- ^ total duration in seconds
  -> [(Double, Double)]    -- ^ black intervals
  -> [(Double, Double)]    -- ^ silence intervals
  -> (Maybe (Double, Double), Maybe (Double, Double))
proposeSegments duration blacks silences
  | duration <= 0 = (Nothing, Nothing)
  | otherwise = (intro, outro)
  where
    merged = mergeIntervals (blacks ++ silences)

    -- longest prefix chain starting within 2 s of 0
    intro = case merged of
      []           -> Nothing
      ((s, _) : _) | s <= 2.0 ->
        let (chainStart, chainEnd) = extendFront merged
        in Just (chainStart, min chainEnd 300)
      _ -> Nothing

    -- longest suffix chain ending within 2 s of the duration
    outro = case reverse merged of
      []           -> Nothing
      ((_, e) : _) | e >= duration - 2.0 ->
        let rev = reverse merged
            (chainEnd, chainStart) = extendFront (map swapI rev)
        in Just (max chainStart (duration - 300), chainEnd)
      _ -> Nothing

    swapI (a, b) = (b, a)

    -- walk a start-sorted interval list, extending the chain while the
    -- next interval starts within 0.5 s of the current end
    extendFront [] = (0, 0)
    extendFront ((s0, e0) : rest) = go s0 e0 rest
      where
        go s e [] = (s, e)
        go s e ((s2, e2) : more)
          | s2 <= e + 0.5 = go s (max e e2) more
          | otherwise     = (s, e)
