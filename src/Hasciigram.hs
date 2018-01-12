module Hasciigram (histogram) where

histogram :: Show s => [(s, Integer)] -> IO ()
histogram bins = do
  let
    longestNameLen = maximum $ map (length . show . fst) bins
    largestValue   = maximum $ map snd bins
  histogram' bins longestNameLen largestValue

histogram' :: Show s => [(s, Integer)] -> Int -> Integer -> IO ()
histogram' bins longestNameLen largestValue = do
  let
    maxBlocks = (79 - longestNameLen) * 8
    normalised = normaliseBins bins largestValue maxBlocks
  printBins normalised longestNameLen

normaliseBins :: [(s, Integer)] -> Integer -> Int -> [(s, Integer)]
normaliseBins [] _ _ = []
normaliseBins bins largestValue maxBlocks =
  map (normaliseBin largestValue maxBlocks) bins

normaliseBin :: Integer -> Int -> (s, Integer) -> (s, Integer)
normaliseBin largestValue maxBlocks (binName, binValue) =
  (binName, v) where
    lv = fromInteger largestValue
    bv = fromInteger binValue
    blockSize = bv / lv
    value = blockSize * fromInteger (toInteger maxBlocks)
    v = floor value

printBins :: Show s => [(s, Integer)] -> Int -> IO ()
printBins [] _ = putStrLn ""
printBins (b:bins) longestNameLen  = do
  let
    name     = (show . fst) b
    namelen  = length name
    binName  = name ++ replicate (longestNameLen - namelen) ' ' ++ "│"
    value    = fromIntegral $ snd b
    binValue = preciseBinValue value
  putStrLn $ binName ++ binValue
  printBins bins longestNameLen

preciseBinValue :: Integer -> String
preciseBinValue v
  | v == 0 = ""
  | v == 1 = eighth
  | v == 2 = quarter
  | v == 3 = threeEights
  | v == 4 = half
  | v == 5 = fiveEights
  | v == 6 = threeQuarters
  | v == 7 = sevenEights
  | otherwise = concat (replicate (fromInteger (v `div` 8)) full) ++ preciseBinValue (v `mod` 8)

full = "█"
sevenEights = "\x2589"
threeQuarters = "\x258A"
fiveEights = "\x258B"
half = "\x258C"
threeEights = "\x258D"
quarter = "\x258E"
eighth = "\x258F"
