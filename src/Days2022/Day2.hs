{-# LANGUAGE OverloadedStrings #-}

module Days2022.Day2 where

import Data.Text.Conversions (FromText (fromText))
import Import

data Hand = Rock | Paper | Scissors
  deriving (Eq, Enum, Bounded, Show)

newtype OponentHand = OponentHand Hand
  deriving (Show)

instance FromText OponentHand where
  fromText "A" = OponentHand Rock
  fromText "B" = OponentHand Paper
  fromText "C" = OponentHand Scissors
  fromText _ = undefined

newtype MyHand = MyHand Hand
  deriving (Show)

instance FromText MyHand where
  fromText "X" = MyHand Rock
  fromText "Y" = MyHand Paper
  fromText "Z" = MyHand Scissors
  fromText _ = undefined

class (Eq a, Enum a, Bounded a) => CyclicEnum a where
  cpred :: a -> a
  cpred c
    | c == minBound = maxBound
    | otherwise = pred c

  csucc :: a -> a
  csucc c
    | c == maxBound = minBound
    | otherwise = succ c

instance CyclicEnum Hand

data MatchResult = First | Second | Draw
  deriving (Show, Eq)

getMatchResult :: Hand -> Hand -> MatchResult
getMatchResult firstHand secondHand
  | firstHand == secondHand = Draw
  | firstHand == cpred secondHand = Second
  | firstHand == csucc secondHand = First
  | otherwise = undefined
