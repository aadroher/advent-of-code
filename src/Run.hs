{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import qualified Days2019.Day1 as D1of2019
import qualified Days2019.Day2 as D2of2019
import qualified Days2019.Day3 as D3of2019
import qualified Days2021.Day1 as D1of2021
import qualified Days2021.Day10 as D10of2021
import qualified Days2021.Day2 as D2of2021
import qualified Days2021.Day3 as D3of2021
import qualified Days2021.Day4 as D4of2021
import qualified Days2021.Day5 as D5of2021
import qualified Days2021.Day6 as D6of2021
import qualified Days2021.Day7 as D7of2021
import qualified Days2021.Day8 as D8of2021
import qualified Days2021.Day9 as D9of2021
import Import
import Util (getResult)

getSolver :: String -> (FilePath -> IO Text)
getSolver "19-1-1" = D1of2019.calculateFirstResult
getSolver "19-1-2" = D1of2019.calculateSecondResult
getSolver "19-2-1" = D2of2019.calculateFirstResult
getSolver "19-2-2" = D2of2019.calculateSecondResult
getSolver "19-3-1" = D3of2019.calculateFirstResult
getSolver "21-1-1" = D1of2021.calculateFirstResult
getSolver "21-1-2" = D1of2021.calculateSecondResult
getSolver "21-2-1" = D2of2021.calculateFirstResult
getSolver "21-2-2" = D2of2021.calculateSecondResult
getSolver "21-3-1" = D3of2021.calculateFirstResult
getSolver "21-3-2" = D3of2021.calculateSecondResult
getSolver "21-4-1" = D4of2021.calculateFirstResult
getSolver "21-4-2" = D4of2021.calculateSecondResult
getSolver "21-5-1" = D5of2021.calculateFirstResult
getSolver "21-5-2" = D5of2021.calculateSecondResult
getSolver "21-6-1" = D6of2021.calculateFirstResult
getSolver "21-6-2" = D6of2021.calculateSecondResult
getSolver "21-7-1" = D7of2021.calculateFirstResult
getSolver "21-7-2" = D7of2021.calculateSecondResult
getSolver "21-8-1" = D8of2021.calculateFirstResult
getSolver "21-8-2" = D8of2021.calculateSecondResult
getSolver "21-9-1" = D9of2021.calculateFirstResult
getSolver "21-10-1" = D10of2021.calculateFirstResult
getSolver _ = undefined

run :: RIO App ()
run = do
  logInfo "Hohoho, the Advent of Code!"
  app <- ask
  let e = exerciseName $ appOptions app
  logInfo $ displayShow $ "Results for day " ++ e ++ ":"
  res1 <- liftIO $ getResult (getSolver e) e
  logInfo $ display res1