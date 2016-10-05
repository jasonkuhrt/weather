{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

{- Example "data-point" from a "daily" result:

    "time":1475564400,
    "summary":"Partly cloudy until evening.",
    "icon":"partly-cloudy-day",
    "sunriseTime":1475590177,
    "sunsetTime":1475632150,
    "moonPhase":0.12,
    "precipIntensity":0,
    "precipIntensityMax":0,
    "precipProbability":0,
    "temperatureMin":55.22,
    "temperatureMinTime":1475647200,
    "temperatureMax":68.38,
    "temperatureMaxTime":1475622000,
    "apparentTemperatureMin":55.22,
    "apparentTemperatureMinTime":1475647200,
    "apparentTemperatureMax":68.38,
    "apparentTemperatureMaxTime":1475622000,
    "dewPoint":51.8,
    "humidity":0.75,
    "windSpeed":6.3,
    "windBearing":311,
    "visibility":10,
    "cloudCover":0.42,
    "pressure":1016,
    "ozone":290.35
-}


module WeatherPoint (
  CurrentWeather(..),
  WeatherPoint(..)
) where

import Data.Text (Text)
import Data.Aeson
import Data.ByteString.Lazy



data WeatherPoint = WeatherPoint {
    summary           :: Text,
    icon              :: Text,
    temperatureMin    :: Double, -- C or F
    temperatureMax    :: Double, -- C or F
    humidity          :: Double, -- 0-1, percentage
    precipProbability :: Double  -- 0-1, percentage
  } deriving (Eq, Show)

instance FromJSON WeatherPoint where

  parseJSON (Object v) =
    WeatherPoint
    <$> v .: "summary"
    <*> v .: "icon"
    <*> v .: "temperatureMin"
    <*> v .: "temperatureMax"
    <*> v .: "humidity"
    <*> v .: "precipProbability"

  parseJSON _          = mempty



data CurrentWeather = CurrentWeather {
      summary           :: Text,
      icon              :: Text,
      temperature       :: Double, -- C or F
      humidity          :: Double, -- 0-1, percentage
      precipProbability :: Double  -- 0-1, percentage
  } deriving (Eq, Show)

instance FromJSON CurrentWeather where

  parseJSON (Object v) =
    CurrentWeather
    <$> v .: "summary"
    <*> v .: "icon"
    <*> v .: "temperature"
    <*> v .: "humidity"
    <*> v .: "precipProbability"

  parseJSON _          = mempty



sample :: ByteString
sample = "{\"time\":1475650800,\"summary\":\"Clear throughout the day.\",\"icon\":\"clear-day\",\"sunriseTime\":1475676631,\"sunsetTime\":1475718459,\"moonPhase\":0.15,\"precipIntensity\":0,\"precipIntensityMax\":0,\"precipProbability\":0,\"temperatureMin\":54.79,\"temperatureMinTime\":1475676000,\"temperatureMax\":70.74,\"temperatureMaxTime\":1475704800,\"apparentTemperatureMin\":54.79,\"apparentTemperatureMinTime\":1475676000,\"apparentTemperatureMax\":70.74,\"apparentTemperatureMaxTime\":1475704800,\"dewPoint\":49.47,\"humidity\":0.68,\"windSpeed\":8.4,\"windBearing\":315,\"visibility\":9.36,\"cloudCover\":0.07,\"pressure\":1017.49,\"ozone\":310.41}"
