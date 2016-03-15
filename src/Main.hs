{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe
import Network.Wreq
import Control.Lens
import Data.Aeson.Lens
import Forecast



accessToken = "db4db26b0bb9053853a23e8565ce7be9"
getWeatherIn = getWeather accessToken



main :: IO ()
main = do
  r <- getWeatherIn montreal
  let summary = fromJust $ r ^? responseBody . key "hourly" . key "summary"
  print summary
