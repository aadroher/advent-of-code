{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021.Day6 where

import Import
import qualified RIO.HashMap as HM
import RIO.HashMap.Partial ((!))
import qualified RIO.List as L
import qualified RIO.List.Partial as L'
import RIO.Partial (read)
import qualified RIO.Text as T
import Util (calculateResult)

type Fish = Int

type FishSchool = [Fish]

type School = HashMap Int Int

reproductionPeriod :: Int
reproductionPeriod = 7

emptySchool :: School
emptySchool =
  HM.fromList
    [ (0, 0),
      (1, 0),
      (2, 0),
      (3, 0),
      (4, 0),
      (5, 0),
      (6, 0),
      (7, 0),
      (8, 0)
    ]

parseSchool :: Text -> School
parseSchool t =
  HM.unionWith (+) emptySchool $
    HM.fromListWith
      (+)
      [ (p, 1) | p <- initialStates
      ]
  where
    initialStates = read . T.unpack <$> T.split (== ',') t

nextStep :: School -> School
nextStep s =
  HM.fromList
    [ (0, s ! 1),
      (1, s ! 2),
      (2, s ! 3),
      (3, s ! 4),
      (4, s ! 5),
      (5, s ! 6),
      (6, s ! 0 + s ! 7),
      (7, s ! 8),
      (8, s ! 0)
    ]

evolve :: School -> Int -> School
evolve s 0 = s
evolve s n = evolve (nextStep s) (n - 1)

numFish :: School -> Int
numFish s = L.sum $ snd <$> HM.toList s

populationOnDayN :: School -> Int -> Int
populationOnDayN s n = numFish $ evolve s n

calculateFirstResult :: FilePath -> IO Text
calculateFirstResult = calculateResult parseSchool (\s -> populationOnDayN (L'.head s) 80)
