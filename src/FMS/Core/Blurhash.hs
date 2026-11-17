module FMS.Core.Blurhash
  ( encodeBlurhash
  , decodeBlurhashSize
  , base83Encode
  , base83Decode
  , base83Alphabet
  ) where

import Data.Bits (shiftL, (.|.))
import Data.Word (Word8)

-- Pure blurhash encoder (Jellyfin BaseItemDto.ImageBlurHashes parity).
-- No package dependencies beyond base: pixels are raw RGB24 bytes
-- (row-major, 3 bytes per pixel) and the output is a base83 String.
-- Follows the reference algorithm (woltapp/blurhash): sRGB -> linear
-- per channel, 2D DCT-II per component per channel, DC packed as 4
-- base83 chars, each AC component packed as ONE 2-char value (the three
-- signed channel quantizations, base 19: 19^3 = 6859 <= 83^2 = 6889).

-- | The base83 alphabet used by blurhash (83 chars, exact order).
base83Alphabet :: String
base83Alphabet =
  "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz#$%*+,-.:;=?@[]^_{|}~"

-- | Encode an integer as exactly @len@ base83 characters (most
-- significant first). @value@ must be in @[0, 83^len - 1]@.
-- | The numeric radix: the alphabet's length.
base83Radix :: Int
base83Radix = length base83Alphabet

base83Encode :: Int -> Int -> String
base83Encode value len = go value len []
  where
    go _ 0 acc = acc
    go v n acc = go (v `div` base83Radix) (n - 1) (base83Alphabet !! (v `mod` base83Radix) : acc)

-- | Decode a base83 string (inverse of 'base83Encode'); unknown
-- characters decode as 0.
base83Decode :: String -> Int
base83Decode = foldl' (\acc c -> acc * base83Radix + idx c) 0
  where
    idx c = case lookup c (zip base83Alphabet [0 ..]) of
      Just i  -> i
      Nothing -> 0

-- | sRGB byte (0-255) to linear-light double (piecewise sRGB EOTF).
srgbToLinear :: Word8 -> Double
srgbToLinear v =
  let c = fromIntegral v / 255
  in if c <= 0.04045
       then c / 12.92
       else ((c + 0.055) / 1.055) ** 2.4

-- | Linear-light double to an sRGB byte (0-255), clamped and rounded.
linearToSrgbByte :: Double -> Int
linearToSrgbByte v =
  let c = if v <= 0.0031308
            then 12.92 * v
            else 1.055 * v ** (1 / 2.4) - 0.055
      i = floor (c * 255 + 0.5)
  in max 0 (min 255 i)

-- | @signum v * |v|^p@ (used for AC quantization).
signPow :: Double -> Double -> Double
signPow v p = signum v * (abs v ** p)

-- | Encode a blurhash.
--
-- @encodeBlurhash width height componentsX componentsY pixels@ where
-- @pixels@ are @width*height*3@ raw RGB24 bytes (row-major). Returns
-- the blurhash string, or @\"\"@ on invalid input (bad dimensions,
-- components outside 1..9, or a pixel count that does not match
-- @width*height*3@).
--
-- Output layout: 1 char for @(componentsX-1) + (componentsY-1)*9@,
-- 1 char for the quantized max AC, 4 chars for the DC (packed
-- linear-to-sRGB bytes), then 2 chars per remaining AC component
-- (row-major over the component grid, DC excluded; each component
-- packs its three signed channel quantizations in base 19).
encodeBlurhash :: Int -> Int -> Int -> Int -> [Word8] -> String
encodeBlurhash w h cx cy pixels
  | w <= 0 || h <= 0 || cx < 1 || cx > 9 || cy < 1 || cy > 9
    || length pixels /= w * h * 3 = ""
  | otherwise =
      base83Encode (cx - 1 + (cy - 1) * 9) 1
      ++ base83Encode quantMax 1
      ++ base83Encode dcPacked 4
      ++ concat [ base83Encode (packAC actualMaxAC comp) 2 | comp <- acComponents ]
  where
    -- linear-light pixels as (r, g, b) triples, row-major
    lin :: [(Double, Double, Double)]
    lin =
      [ (srgbToLinear r, srgbToLinear g, srgbToLinear b)
      | [r, g, b] <- chunksOf3 pixels
      ]
    chunksOf3 [] = []
    chunksOf3 (a : b : c : rest) = [a, b, c] : chunksOf3 rest
    chunksOf3 _ = []

    -- rows of the image, each a list of linear pixels
    rows :: [[(Double, Double, Double)]]
    rows = rowsOf h lin
    rowsOf n xs = case splitAt n xs of
      (row, rest) | not (null row) -> row : rowsOf n rest
      _                            -> []

    -- 2D DCT-II basis per component (cy rows x cx cols); each component
    -- is an (r, g, b) triple of linear values.
    components :: [(Double, Double, Double)]
    components =
      [ (basisAt x y fst3, basisAt x y snd3, basisAt x y thd3)
      | y <- [0 .. cy - 1]
      , x <- [0 .. cx - 1]
      ]
    fst3 (a, _, _) = a
    snd3 (_, b, _) = b
    thd3 (_, _, c) = c
    basisAt x y getCh =
      let norm = if x == 0 && y == 0 then 1 else 2 :: Double
      in norm / fromIntegral (w * h)
         * sum
             [ cos (pi * fromIntegral x * fromIntegral px / fromIntegral w)
               * cos (pi * fromIntegral y * fromIntegral py / fromIntegral h)
               * getCh p
             | (py, row) <- zip [0 ..] rows :: [(Int, [(Double, Double, Double)])]
             , (px, p)   <- zip [0 ..] row :: [(Int, (Double, Double, Double))]
             ]

    dc :: (Double, Double, Double)
    dc = case components of
      (d : _) -> d
      []      -> (0, 0, 0)
    acComponents :: [(Double, Double, Double)]
    acComponents = case components of
      (_ : rest) -> rest
      []         -> []
    compTriple (a, b, c) = [a, b, c]

    dcPacked :: Int
    dcPacked =
      let (r, g, b) = dc
      in  (linearToSrgbByte r `shiftL` 16)
          .|. (linearToSrgbByte g `shiftL` 8)
          .|. (linearToSrgbByte b)

    maxAC :: Double
    maxAC = maximum (0 : [ abs v | comp <- acComponents, v <- compTriple comp ])

    quantMax :: Int
    quantMax = max 0 (min 82 (floor (maxAC * 166)))
    actualMaxAC :: Double
    actualMaxAC = (fromIntegral quantMax + 1) / 166

    -- signed quantization of one AC channel value to -9..9
    quantAC :: Double -> Double -> Int
    quantAC actualMax v =
      max (-9) (min 9 (floor (signPow (v / actualMax) 0.5 * 9 + 0.5)))

    -- pack the three channel quantizations (each shifted to 0..18) in
    -- base 19 into one integer < 83^2, encoded as 2 base83 chars
    packAC :: Double -> (Double, Double, Double) -> Int
    packAC actualMax (r, g, b) =
      let qr = quantAC actualMax r
          qg = quantAC actualMax g
          qb = quantAC actualMax b
      in (qr + 9) * 361 + (qg + 9) * 19 + (qb + 9)

-- | Decode just the component counts from a blurhash's first char:
-- @(componentsX, componentsY)@, or 'Nothing' for an empty string.
decodeBlurhashSize :: String -> Maybe (Int, Int)
decodeBlurhashSize s = case s of
  []      -> Nothing
  (c : _) ->
    let v = base83Decode [c]
    in Just (v `mod` 9 + 1, v `div` 9 + 1)
