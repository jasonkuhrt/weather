-- Note that annother implementation can be found at:
-- https://rosettacode.org/wiki/Sparkline_in_unicode#Haskell

module Sparkline where

import Data.Char



barCount = length bars
bars = map chr [0x2581..0x2588]



draw :: RealFrac a => a -> a -> [a] -> String
draw bot top =
  fmap drawBar
  where
  drawBar = getIndexOf bars . calculateBarIndex barCount bot top



inferredDraw :: RealFrac a => [a] -> String
inferredDraw xs =
  fmap drawBar xs
  where
  drawBar = getIndexOf bars . calculateBarIndex barCount bot top
  bot     = minimum xs
  top     = maximum xs



calculateBarIndex :: (Eq a, RealFrac a) => Int -> a -> a -> a -> Int
calculateBarIndex barCount bot top xAbsolute =
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



getIndexOf xs i = xs !! fromIntegral i



-- Development --

-- The resolution of the Sparkline is 8 steps. All numbers in the set must map to one of these steps.

-- barCount    = 8 (AKA resolution)
-- dataMin      = 0
-- dataMax      = 10
-- dataSize     = dataMax - dataMin
-- dataPerBar  = dataSize / barCount
-- calc         = ceiling . (/ dataPerBar) . subtract dataMin
--
-- e.g.
-- datum        = 3
-- step         = calc datum

test =
  putStrLn .
  unlines $
  [
    draw 0 10 [0..10]
  , inferredDraw [0..10]
  , draw 0 5 [0..5]
  , draw 5 10 [5..10]
  , draw (-1000) 1000 (parse "-1000 100 1000 500 200 -400 -700 621 -189 3")
  , draw 1 8 (parse "1 2 3 4 5 6 7 8 7 6 5 4 3 2 1")
  ]

parse = map (\x -> read x :: Float) . words
