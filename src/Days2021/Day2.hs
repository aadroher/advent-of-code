module Days2021.Day2 where

import Import

type Position = (Int, Int)

data Command = F Int | U Int | D Int
  deriving (Eq, Show)

move :: Position -> [Command] -> Position
move p [] = p
move (x, y) (F n : cs) = move (x + n, y) cs
move (x, y) (U n : cs) = move (x, y - n) cs
move (x, y) (D n : cs) = move (x, y + n) cs
