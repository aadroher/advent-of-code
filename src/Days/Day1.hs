module Days.Day1 where

import RIO
import qualified RIO.Text as T
import Text.Pretty.Simple (pPrint)

getFuelRequirements :: Integer -> Integer
getFuelRequirements n = n `div` 3 - 2

loadFuelRequirements :: FilePath -> IO [Integer]
loadFuelRequirements f = do
  fileContents <- readFileUtf8 f
  let ns = read <$> lines (T.unpack fileContents)
  pure ns

calculateResult :: FilePath -> IO Integer
calculateResult f = do
  fuelRequirements <- loadFuelRequirements f
  pure $ sum $ getFuelRequirements <$> fuelRequirements
