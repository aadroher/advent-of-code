module Days.Day1 where

import RIO
import qualified RIO.Text as T
import Text.Pretty.Simple (pPrint)

getFuelRequirements :: Integer -> Integer
getFuelRequirements n = n `div` 3 - 2

getTotalFuelRequirements :: Integer -> Integer
getTotalFuelRequirements n =
  if fr > 0
    then fr + getTotalFuelRequirements fr
    else max fr 0
  where
    fr = getFuelRequirements n

loadFuelRequirements :: FilePath -> IO [Integer]
loadFuelRequirements f = do
  fileContents <- readFileUtf8 f
  let ns = read <$> lines (T.unpack fileContents)
  pure ns

calculateResult :: (Integer -> Integer) -> FilePath -> IO Integer
calculateResult f p = do
  result <- loadFuelRequirements p
  pure $ sum $ f <$> result

calculateFirstResult :: FilePath -> IO Integer
calculateFirstResult = calculateResult getFuelRequirements

calculateSecondResult :: FilePath -> IO Integer
calculateSecondResult = calculateResult getTotalFuelRequirements
