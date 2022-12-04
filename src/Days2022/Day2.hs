{-# LANGUAGE OverloadedStrings #-}

module Days2022.Day2 where

import Data.Text.Conversions (FromText (fromText))
import Import
import qualified RIO.List as L
import qualified RIO.Text as T
import Util (calculateResult)

data Hand = Rock | Paper | Scissors
  deriving (Eq, Enum, Bounded, Show)

newtype OponentHand = OponentHand Hand
  deriving (Show, Eq)

instance FromText OponentHand where
  fromText "A" = OponentHand Rock
  fromText "B" = OponentHand Paper
  fromText "C" = OponentHand Scissors
  fromText _ = undefined

newtype MyHand = MyHand Hand
  deriving (Show, Eq)

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

instance FromText MatchResult where
  fromText "X" = First
  fromText "Y" = Draw
  fromText "Z" = Second
  fromText _ = undefined

type Match = (OponentHand, MyHand)

type MatchPrescription = (OponentHand, MatchResult)

handToScore :: Hand -> Int
handToScore Rock = 1
handToScore Paper = 2
handToScore Scissors = 3

matchResultToScore :: MatchResult -> Int
matchResultToScore First = 0
matchResultToScore Draw = 3
matchResultToScore Second = 6

getMatchResult :: Hand -> Hand -> MatchResult
getMatchResult firstHand secondHand
  | firstHand == secondHand = Draw
  | firstHand == cpred secondHand = Second
  | firstHand == csucc secondHand = First
  | otherwise = undefined

getMatchScore :: Match -> Int
getMatchScore (OponentHand opponentHand, MyHand myHand) =
  handToScore myHand + matchResultToScore (getMatchResult opponentHand myHand)

getMatchesScore :: [Match] -> Int
getMatchesScore ms = L.sum $ getMatchScore <$> ms

getMyHandPrescription :: MatchPrescription -> MyHand
getMyHandPrescription (OponentHand oponentHand, mr) =
  case mr of
    First -> MyHand $ cpred oponentHand
    Draw -> MyHand oponentHand
    Second -> MyHand $ csucc oponentHand

getPrescribedMatch :: MatchPrescription -> Match
getPrescribedMatch mp@(oh, _) = (oh, getMyHandPrescription mp)

getPrescriptionsScore :: [MatchPrescription] -> Int
getPrescriptionsScore mps = L.sum $ (getMatchScore . getPrescribedMatch) <$> mps

parseMatch :: Text -> Match
parseMatch t =
  ( fromText $ T.take 1 t,
    fromText $ T.drop 2 t
  )

parseMatchPrescription :: Text -> MatchPrescription
parseMatchPrescription t =
  ( fromText $ T.take 1 t,
    fromText $ T.drop 2 t
  )

calculateFirstResult :: FilePath -> IO Text
calculateFirstResult = calculateResult parseMatch getMatchesScore