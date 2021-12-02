module Days2021.Day1 where

import Import
import qualified RIO.List as L
import qualified RIO.Text as T

compareInitialPair :: [Int] -> Int
compareInitialPair [] = 0
compareInitialPair [_] = 0
compareInitialPair (x0 : x1 : _) =
  if x0 < x1
    then 1
    else 0

countIncreases :: [Int] -> Int
countIncreases xs =
  L.sum $ L.map compareInitialPair (L.tails xs)

loadReadings :: FilePath -> IO [Int]
loadReadings f = do
  fileContents <- readFileUtf8 f
  let ns = read <$> lines (T.unpack fileContents)
  pure ns

calculateResult :: ([Int] -> Int) -> FilePath -> IO Text
calculateResult f p = do
  xs <- loadReadings p
  pure $ (T.pack . show) $ f xs

calculateFirstResult :: FilePath -> IO Text
calculateFirstResult = calculateResult countIncreases