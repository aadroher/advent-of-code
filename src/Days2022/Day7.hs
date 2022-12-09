{-# LANGUAGE OverloadedStrings #-}

module Days2022.Day7 where

import Import
import qualified RIO.List as L
import qualified RIO.List.Partial as L'
import qualified RIO.Text as T
import Util (calculateResult)

data DirReference = Root | Parent | Child !Text
  deriving (Eq, Show)

data Command = Cd !DirReference | Ls
  deriving (Eq, Show)

data Node = Dir !Text | File !Int !Text
  deriving (Eq, Show)

data ParsedLine = ParsedCommand !Command | ParsedNode !Node
  deriving (Eq, Show)

parseLine :: Text -> ParsedLine
parseLine = undefined