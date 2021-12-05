{-# LANGUAGE OverloadedStrings #-}

module Days2021.Day3 where

import Data.Tuple.Select as S
import Import
import qualified RIO.List as L
import qualified RIO.Text as T

data Bit = Zero | One
  deriving (Eq, Show)

data RateType = Gamma | Epsilon

type BinNum = (Bit, Bit, Bit, Bit, Bit)

bitToFloat :: Floating a => Bit -> a
bitToFloat Zero = 0.0
bitToFloat One = 1.0

getCommonalityByComparator :: Floating a => (a -> a -> Bool) -> [Bit] -> Bit
getCommonalityByComparator comp bs =
  let avg = sum (bitToFloat <$> bs) / fromIntegral (L.length bs)
   in bool Zero One (avg `comp` 0.5)

getLeastCommon :: [Bit] -> Bit
getLeastCommon = getCommonalityByComparator (>)

getMostCommon :: [Bit] -> Bit
getMostCommon = getCommonalityByComparator (<)

getColumnAt :: Int -> [BinNum] -> [Bit]
getColumnAt i bns = sel <$> bns
  where
    sel = case i of
      0 -> S.sel1
      1 -> S.sel2
      2 -> S.sel3
      3 -> S.sel4
      4 -> S.sel5
      _ -> error "Index out of bounds"

calculateRate :: ([Bit] -> Bit) -> [BinNum] -> BinNum
calculateRate agg bs =
  ( colAgg 0,
    colAgg 1,
    colAgg 2,
    colAgg 3,
    colAgg 4
  )
  where
    colAgg = \i -> agg $ getColumnAt i bs

calculateGammaRate :: [BinNum] -> BinNum
calculateGammaRate = calculateRate getMostCommon

calculateEpsilonRate :: [BinNum] -> BinNum
calculateEpsilonRate = calculateRate getLeastCommon

parseBit :: Char -> Bit
parseBit '0' = Zero
parseBit '1' = One
parseBit _ = undefined

parseBinNum :: Text -> BinNum
parseBinNum t = (b0, b1, b2, b3, b4)
  where
    [b0, b1, b2, b3, b4] = parseBit <$> T.unpack t
