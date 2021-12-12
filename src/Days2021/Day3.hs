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

bitToIntegral :: Integral a => Bit -> a
bitToIntegral Zero = 0
bitToIntegral One = 1

binNumToInt :: BinNum -> Int
binNumToInt bn =
  sum $
    (\(i, b) -> bitToIntegral b * 2 ^ i)
      <$> zip [0 ..] (L.reverse bn)

countOccurrences :: [Bit] -> (Int, Int)
countOccurrences bs = (numZeroes, numOnes)
  where
    numOnes = L.sum (bitToIntegral <$> bs)
    numZeroes = L.length bs - numOnes

getLeastCommon :: [Bit] -> Bit
getLeastCommon bs =
  if numZeroes <= numOnes
    then Zero
    else One
  where
    (numZeroes, numOnes) = countOccurrences bs

getMostCommon :: [Bit] -> Bit
getMostCommon bs =
  if numZeroes <= numOnes
    then One
    else Zero
  where
    (numZeroes, numOnes) = countOccurrences bs

getColumnAt :: Int -> [BinNum] -> [Bit]
getColumnAt i bns = (!! i) <$> bns

calculateRate :: ([Bit] -> Bit) -> [BinNum] -> BinNum
calculateRate agg bs = colAgg <$> L.init [0 .. colWidth]
  where
    colAgg = \i -> agg $ getColumnAt i bs
    colWidth = L.length $ L'.head bs

getGammaRate :: [BinNum] -> BinNum
getGammaRate = calculateRate getMostCommon

getEpsilonRate :: [BinNum] -> BinNum
getEpsilonRate = calculateRate getLeastCommon

getRatesProduct :: [BinNum] -> Int
getRatesProduct bs = L.product $ binNumToInt <$> [gamma, epsilon]
  where
    gamma = getGammaRate bs
    epsilon = getEpsilonRate bs

filterByMostCommon :: Int -> [BinNum] -> [BinNum]
filterByMostCommon _ [bn] = [bn]
filterByMostCommon i bns =
  filterByMostCommon (i + 1) $
    L.filter (\bn -> bn !! i == mostCommon) bns
  where
    gamma = getGammaRate bns
    mostCommon = gamma !! i

getOxigenGeneratorRating :: [BinNum] -> BinNum
getOxigenGeneratorRating = L'.head . filterByMostCommon 0

filterByLeastCommon :: Int -> [BinNum] -> [BinNum]
filterByLeastCommon _ [bn] = [bn]
filterByLeastCommon i bns =
  filterByLeastCommon (i + 1) $
    L.filter (\bn -> bn !! i == mostCommon) bns
  where
    epsilon = getEpsilonRate bns
    mostCommon = epsilon !! i

getCO2ScrubberRating :: [BinNum] -> BinNum
getCO2ScrubberRating = L'.head . filterByLeastCommon 0

getRatingsProduct :: [BinNum] -> Int
getRatingsProduct bs = L.product $ binNumToInt <$> [oxigenGenerator, co2scrubber]
  where
    oxigenGenerator = getOxigenGeneratorRating bs
    co2scrubber = getCO2ScrubberRating bs

calculateFirstResult :: FilePath -> IO Text
calculateFirstResult = calculateResult parseBinNum getRatesProduct

calculateSecondResult :: FilePath -> IO Text
calculateSecondResult = calculateResult parseBinNum getRatingsProduct
