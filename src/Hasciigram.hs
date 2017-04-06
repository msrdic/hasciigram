module Hasciigram where

histogram :: (Show s, Num a, Ord a, Show a) => [(s, a)] -> IO ()
histogram bins = do
  let longestName = maximum $ map (length . show . fst) bins
      largestValue = maximum $ map snd bins
  histogram' bins longestName largestValue

histogram' :: (Show s, Num a, Ord a, Show a) => [(s, a)] -> Int -> a -> IO ()
histogram' bins longestName largestValue = do
  let normalised = normaliseBins bins largestValue
  printBins normalised longestName largestValue

normaliseBins bins v = bins

printBins :: (Show s, Num a, Ord a, Show a) => [(s, a)] -> Int -> a -> IO ()
printBins (b:bins) longestName largestValue  = do
  -- not 80, but 79, because we reserve one for delimiter
  let binValueLength = 79 - longestName

  let name     = (show . fst) b
      namelen  = length name
      binName  = name ++ (replicate (longestName - namelen) ' ') ++ "|"

  let value    = normaliseValue (snd b) largestValue
      binValue = show value

  putStrLn $ binName ++ binValue
  printBins bins longestName largestValue
printBins [] _ _ = putStrLn ""

normaliseValue :: Num a => a -> a -> a
normaliseValue n max = n
