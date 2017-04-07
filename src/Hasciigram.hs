module Hasciigram where

histogram :: Show s => [(s, Integer)] -> IO ()
histogram bins = do
  let longestNameLen = maximum $ map (length . show . fst) bins
      largestValue   = maximum $ map snd bins
  histogram' bins longestNameLen largestValue

histogram' :: Show s => [(s, Integer)] -> Int -> Integer -> IO ()
histogram' bins longestNameLen largestValue = do
  let maxBlocks = 79 - longestNameLen
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
    value = blockSize * (fromInteger (toInteger maxBlocks))
    v = floor value

printBins :: Show s => [(s, Integer)] -> Int -> IO ()
printBins (b:bins) longestNameLen  = do

  let name     = (show . fst) b
      namelen  = length name
      binName  = name ++ (replicate (longestNameLen - namelen) ' ') ++ "|"

  let value    = fromIntegral $ snd b
      binValue = replicate value '█'

  putStrLn $ binName ++ binValue
  printBins bins longestNameLen
printBins [] _ = putStrLn ""
