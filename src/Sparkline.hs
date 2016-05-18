module Sparkline where

import Data.Char

{-# ANN module "HLint: ignore Eta reduce" #-}

-- Note that annother implementation can be found at:
-- https://rosettacode.org/wiki/Sparkline_in_unicode#Haskell



bars :: String
bars = map chr [0x2581..0x2588]



-- API --

draw :: RealFrac a => [a] -> String
draw xs = drawWithRange (calcRange xs) xs

drawWithRange :: RealFrac a => (a,a) -> [a] -> String
drawWithRange range xs =
  fmap drawBar xs
  where
  drawBar = getIndexOf bars . calculateBarIndex barCount range
  barCount = length bars



-- Helpers --

calculateBarIndex :: (Eq a, RealFrac a) => Int -> (a,a) -> a -> Int
calculateBarIndex barCount (bot,top) xAbsolute =
  barAtPercentage dataPercent
  where
  -- Given the datum's percentage placement within the data range
  -- we can then find the corresponding bar at that percent. Of course
  -- this needs to be rounded since bars are a list. In other words the data
  -- percentage is mapped to the **closest matching** bar, not necessarially
  -- a perfect fit. Finally we subtract 1 because we want to be zero-indexed.
  barAtPercentage = round . (* realToFrac (barCount - 1))
  dataPercent     = xRelative / dataSize
  dataSize        = top - bot
  xRelative       = xAbsolute - bot



calcRange :: (Ord a, Num a) => [a] -> (a,a)
calcRange xs = (minimum xs, maximum xs)



getIndexOf :: [a] -> Int -> a
getIndexOf xs i = xs !! fromIntegral i
