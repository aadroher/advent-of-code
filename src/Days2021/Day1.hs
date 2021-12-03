module Days2021.Day1 where

import Import
import qualified RIO.List as L
import qualified RIO.Text as T

generateWindows :: [Int] -> [(Int, Int, Int)]
generateWindows [] = []
generateWindows xs =
  w : generateWindows (tail xs)
  where
    w = case take 3 xs of
      [a, b, c] -> (a, b, c)
      [a, b] -> (a, b, 0)
      [a] -> (a, 0, 0)
      _ -> (0, 0, 0)

compareInitialPair :: [Int] -> Int
compareInitialPair [] = 0
compareInitialPair [_] = 0
compareInitialPair (x0 : x1 : _) = bool 0 1 (x0 < x1)

countWindowIncreases :: [Int] -> Int
countWindowIncreases xs =
  countIncreases ws
  where
    ws = (\(a, b, c) -> L.sum [a, b, c]) <$> generateWindows xs

countIncreases :: [Int] -> Int
countIncreases xs =
  L.sum $ compareInitialPair <$> L.tails xs

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

calculateSecondResult :: FilePath -> IO Text
calculateSecondResult = calculateResult countWindowIncreases