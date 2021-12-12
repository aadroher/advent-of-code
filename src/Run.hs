{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import qualified Days2019.Day1 as D1of2019
import qualified Days2019.Day2 as D2of2019
import qualified Days2019.Day3 as D3of2019
import qualified Days2021.Day1 as D1of2021
import qualified Days2021.Day2 as D2of2021
import qualified Days2021.Day3 as D3of2021
import qualified Days2021.Day4 as D4of2021
import Import
import qualified RIO.List as L
import qualified RIO.Text as T

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
getSolver _ = undefined

getFilePath :: String -> FilePath
getFilePath exName = "./data/" ++ year ++ "/day" ++ num ++ ".txt"
  where
    year = L.take 2 exName
    num = (L.drop 3 . L.take 4) exName

getResult :: String -> RIO App Text
getResult exName = do
  res <- liftIO $ getSolver exName $ getFilePath exName
  pure $ T.pack $ show res

run :: RIO App ()
run = do
  logInfo "Hohoho, the Advent of Code!"
  app <- ask
  let e = exerciseName $ appOptions app
  logInfo $ displayShow $ "Results for day " ++ e ++ ":"
  res1 <- getResult e
  logInfo $ display res1