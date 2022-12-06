{-# LANGUAGE OverloadedStrings #-}

module Days2022.Day3 where

import Import
import qualified RIO.List as L
import qualified RIO.List.Partial as L'
import qualified RIO.Text as T
import Util (calculateResult)

newtype Item = Item Char
  deriving (Eq, Show)

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

parseContentList :: Text -> [Item]
parseContentList t = Item <$> T.unpack t

calculateFirstResult :: FilePath -> IO Text
calculateFirstResult = calculateResult parseContentList getRepeatedItemsPrioritySum
