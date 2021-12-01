module Days2019.Day1 where

import RIO
import qualified RIO.Text as T

getFuelRequirements :: Integer -> Integer
getFuelRequirements n = n `div` 3 - 2

getTotalFuelRequirements :: Integer -> Integer
getTotalFuelRequirements n =
  if fr > 0
    then fr + getTotalFuelRequirements fr
    else max fr 0
  where
    fr = getFuelRequirements n

loadMassess :: FilePath -> IO [Integer]
loadMassess f = do
  fileContents <- readFileUtf8 f
  let ns = read <$> lines (T.unpack fileContents)
  pure ns

calculateResult :: (Integer -> Integer) -> FilePath -> IO Text
calculateResult f p = do
  result <- loadMassess p
  pure $ (T.pack . show) $ sum $ f <$> result

calculateFirstResult :: FilePath -> IO Text
calculateFirstResult = calculateResult getFuelRequirements

calculateSecondResult :: FilePath -> IO Text
calculateSecondResult = calculateResult getTotalFuelRequirements
