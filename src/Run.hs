{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Run (run) where

import qualified Days.Day1 as D1
import Import
import qualified RIO.List as L
import qualified RIO.Partial as P
import qualified RIO.Text as T

getSolver :: String -> (FilePath -> IO Integer)
getSolver "1.1" = D1.calculateFirstResult
getSolver "1.2" = D1.calculateSecondResult
getSolver _ = undefined

getFilePath :: String -> Maybe FilePath
getFilePath exName = (\num -> "./data/day" ++ [num] ++ ".txt") <$> L.headMaybe exName

getResult :: String -> RIO App Text
getResult exName = do
  res <- liftIO $ P.fromJust $ getSolver exName <$> getFilePath exName
  pure $ T.pack $ show res

run :: RIO App ()
run = do
  logInfo "Hohoho, the Advent of Code!"
  app <- ask
  let e = exerciseName $ appOptions app
  logInfo $ displayShow $ "Results for day " ++ e ++ ":"
  res1 <- getResult e
  logInfo $ displayShow res1