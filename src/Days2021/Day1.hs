module Days2021.Day1 where

import Import
import qualified RIO.List as L
import qualified RIO.Text as T
import Util (calculateResult)

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
compareInitialPair (x0 : x1 : _) = bool 0 1 (x0 < x1)
compareInitialPair _ = 0

countWindowIncreases :: [Int] -> Int
countWindowIncreases xs =
  countIncreases ws
  where
    ws = (\(a, b, c) -> L.sum [a, b, c]) <$> generateWindows xs

countIncreases :: [Int] -> Int
countIncreases xs =
  L.sum $ compareInitialPair <$> L.tails xs

calculateFirstResult :: FilePath -> IO Text
calculateFirstResult = calculateResult (read . T.unpack) countIncreases

calculateSecondResult :: FilePath -> IO Text
calculateSecondResult = calculateResult (read . T.unpack) countWindowIncreases