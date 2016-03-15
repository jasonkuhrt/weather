{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wreq
import Control.Lens
import Data.Aeson.Lens
import Data.Maybe
import Data.ByteString.Lazy



type AccessToken = String

data Location =
  Location {
    latitude :: Float,
    longitude :: Float
  }
  deriving (Show, Eq)



-- Forcast.io API

apiHost = "https://api.forecast.io/forecast/"

getWeather :: AccessToken -> Location -> IO (Response ByteString)
getWeather accessToken (Location lat lon) = get uri
  where
    uri = apiHost ++ accessToken ++ "/" ++ (show lat) ++ "," ++ (show lon)

-- Location Aliases

montreal = Location 45.501689 (-73.567256)



-- API Helpers

accessToken = "db4db26b0bb9053853a23e8565ce7be9"
getWeatherIn = getWeather accessToken






main :: IO ()
main = do
  r <- getWeatherIn montreal
  let summary = fromJust $ r ^? responseBody . key "hourly" . key "summary"
  print summary
