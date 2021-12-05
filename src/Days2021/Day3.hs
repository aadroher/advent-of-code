{-# LANGUAGE OverloadedStrings #-}

module Days2021.Day3 where

import qualified Data.List as L
import qualified Data.List as L'
import Data.Tuple.Select as S
import Import
import qualified RIO.List as L
import qualified RIO.List.Partial ((!!))
import qualified RIO.Text as T
import Util (calculateResult)

data Bit = Zero | One
  deriving (Eq, Show)

type BinNum = [Bit]

parseBit :: Char -> Bit
parseBit '0' = Zero
parseBit '1' = One
parseBit _ = undefined

parseBinNum :: Text -> BinNum
parseBinNum t = parseBit <$> T.unpack t

bitToFloat :: Floating a => Bit -> a
bitToFloat Zero = 0.0
bitToFloat One = 1.0

binNumToInt :: BinNum -> Int
binNumToInt bn =
  sum $
    (\(i, b) -> round (bitToFloat b) * 2 ^ i)
      <$> zip [0 ..] (L.reverse bn)

getCommonalityByComparator :: Floating a => (a -> a -> Bool) -> [Bit] -> Bit
getCommonalityByComparator comp bs =
  let avg = sum (bitToFloat <$> bs) / fromIntegral (L.length bs)
   in bool Zero One (avg `comp` 0.5)

getLeastCommon :: [Bit] -> Bit
getLeastCommon = getCommonalityByComparator (<)

getMostCommon :: [Bit] -> Bit
getMostCommon = getCommonalityByComparator (>)

getColumnAt :: Int -> [BinNum] -> [Bit]
getColumnAt i bns = (!! i) <$> bns

calculateRate :: ([Bit] -> Bit) -> [BinNum] -> BinNum
calculateRate agg bs =
  let colAgg = \i -> agg $ getColumnAt i bs
   in let colWidth = L.length $ L'.head bs
       in (colAgg <$> L.init [0 .. colWidth])

getGammaRate :: [BinNum] -> BinNum
getGammaRate = calculateRate getMostCommon

getEpsilonRate :: [BinNum] -> BinNum
getEpsilonRate = calculateRate getLeastCommon

getRatesProduct :: [BinNum] -> Int
getRatesProduct bs = gamma * epsilon
  where
    gamma = binNumToInt $ getGammaRate bs
    epsilon = binNumToInt $ getEpsilonRate bs

calculateFirstResult :: FilePath -> IO Text
calculateFirstResult = calculateResult parseBinNum getRatesProduct
