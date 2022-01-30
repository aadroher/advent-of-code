{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Days2021.Day11 where

import Import
import qualified RIO.List as L
import qualified RIO.Partial as P
import qualified RIO.Text as T

data Octopus = Level Int | Flash

gridSize :: Int
gridSize = 10

parseOctopus :: String -> Octopus
parseOctopus = Level . P.read

parseRow :: Text -> [Octopus]
parseRow t = parseOctopus . (: "") <$> T.unpack t

parseGrid :: Text -> [[Octopus]]
parseGrid t =
  if numRowsIsCorrect && numColsIsCorrect
    then parsedRows
    else error "Incorrect size"
  where
    parsedRows = parseRow <$> T.lines t
    numRowsIsCorrect = L.length parsedRows == gridSize
    numColsIsCorrect = L.all ((== gridSize) . L.length) parsedRows