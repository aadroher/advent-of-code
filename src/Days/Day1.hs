module Days.Day1 where

import RIO
import qualified RIO.Text as T

getFuelRequirements :: Integer -> Integer
getFuelRequirements n = n `div` 3 - 2

loadFuelRequirements :: FilePath -> IO [Integer]
loadFuelRequirements f = do
  fileContents <- readFileUtf8 f
  pure (read <$> lines (T.unpack fileContents))

calculateResult :: FilePath -> IO Integer
calculateResult f = do
  fuelRequirements <- loadFuelRequirements f
  pure $ sum fuelRequirements
