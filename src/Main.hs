{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Maybe
import Data.Monoid
import Network.Wreq
import Data.ByteString.Lazy (ByteString)
import Forecast
import qualified Icons



accessToken = "db4db26b0bb9053853a23e8565ce7be9"
fetchWeatherIn = getWeather accessToken

main :: IO ()
main = do
  r <- fetchWeatherIn montreal
  putStrLn . getWeatherTypeIcon . getWeatherType $ r



getWeatherTypeIcon :: String -> String
getWeatherTypeIcon "rain" = Icons.cloud
getWeatherTypeIcon kind = "Unknown type of weather: " <> kind
