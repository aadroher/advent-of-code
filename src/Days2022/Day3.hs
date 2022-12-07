{-# LANGUAGE OverloadedStrings #-}

module Days2022.Day3 where

import Import
import qualified RIO.List as L
import qualified RIO.List.Partial as L'
import qualified RIO.Text as T
import Util (calculateResult)

newtype Item = Item Char
  deriving (Eq, Show)

type RucksackGroup = ([Item], [Item], [Item])

getDuplicatedItem :: [Item] -> Item
getDuplicatedItem items =
  let midPoint = L.length items `div` 2
      (firstCompartent, secondCompartment) = L.splitAt midPoint items
      intersection = L.intersect firstCompartent secondCompartment
   in L'.head intersection

getItemPriority :: Item -> Int
getItemPriority (Item c) =
  let charsWithPrio = L.zip (['a' .. 'z'] ++ ['A' .. 'Z']) [1 ..]
      [(_, priority)] = L.filter ((c ==) . fst) charsWithPrio
   in priority

getRepeatedItemsPrioritySum :: [[Item]] -> Int
getRepeatedItemsPrioritySum iss =
  L.sum $ getItemPriority . getDuplicatedItem <$> iss

numRucksacksPerGroup :: Int
numRucksacksPerGroup = 3

belongToSameGroup :: (Int, [Item]) -> (Int, [Item]) -> Bool
belongToSameGroup (i, _) (j, _) =
  let trucateByGroupSize = (* numRucksacksPerGroup) . (`div` numRucksacksPerGroup)
   in trucateByGroupSize i == trucateByGroupSize j

toRucksackGroup :: [[Item]] -> RucksackGroup
toRucksackGroup [a, b, c] = (a, b, c)
toRucksackGroup _ = undefined

getRuckSackGroups :: [[Item]] -> [RucksackGroup]
getRuckSackGroups itemLists =
  let withIndex = L.zip [0 ..] itemLists
      groupedIndexedItemLists = L.groupBy belongToSameGroup withIndex
      groupedItemLists = (snd <$>) <$> groupedIndexedItemLists
   in toRucksackGroup <$> groupedItemLists

getBadge :: RucksackGroup -> Item
getBadge (g0, g1, g2) =
  let intersection = g0 `L.intersect` g1 `L.intersect` g2
   in L'.head intersection

getBadgesPrioritySum :: [[Item]] -> Int
getBadgesPrioritySum iss =
  L.sum $ getItemPriority . getBadge <$> getRuckSackGroups iss

parseContentList :: Text -> [Item]
parseContentList t = Item <$> T.unpack t

calculateFirstResult :: FilePath -> IO Text
calculateFirstResult = calculateResult parseContentList getRepeatedItemsPrioritySum

calculateSecondResult :: FilePath -> IO Text
calculateSecondResult = calculateResult parseContentList getBadgesPrioritySum