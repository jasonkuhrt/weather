{-# LANGUAGE OverloadedStrings #-}

-- forecast.io API

module Forecast where

import Data.ByteString.Lazy
import Network.Wreq
import Control.Lens
import Data.Aeson.Lens
import Data.Text



type AccessToken = String

data Location =
  Location {
    latitude :: Float,
    longitude :: Float
  }
  deriving (Show, Eq)



apiHost = "https://api.forecast.io/forecast/"

getWeather :: AccessToken -> Location -> IO (Response ByteString)
getWeather accessToken (Location lat lon) = get uri
  where
  uri = apiHost ++ accessToken ++ "/" ++ (show lat) ++ "," ++ (show lon)


getWeatherType :: (Response ByteString) -> String
getWeatherType r =
  Data.Text.unpack (r ^. responseBody . key "currently" . key "icon" . _String)

-- Location Aliases

montreal = Location 45.501689 (-73.567256)
