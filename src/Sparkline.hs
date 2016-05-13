module Sparkline where

import Data.Char



type Min = Int
type Max = Int

charScale :: String
charScale = map chr [0x2581..0x2588]
resolution = length charScale

render :: Max -> Min -> [Float] -> String
render maxX minX =
  map ((charScale !!) . subtract 1 . doCalcStep)
  -- show . map doCalcStep
  where
  doCalcStep :: Float -> Int
  doCalcStep = calcStep resolution maxX minX

-- Visualize a set of numbers as a Sparkline.
-- The resolution of the Sparkline is 8 steps. All numbers in the set must map to one of these steps.

calcStep :: Int -> Max -> Min -> Float -> Int
calcStep stepCount maxX minX x = doCalc x
  where
  doCalc :: Float -> Int
  doCalc x
    | x == realToFrac maxX  = stepCount
    | otherwise             = (+1) . floor . (/ stepSize) $ x
  stepSize :: Float
  stepSize = (/) (realToFrac maxX) (realToFrac stepCount)



-- Example:
-- resolution = 8 (AKA stepCount)
-- Max        = 10
-- Min        = 0
-- value      = 3
-- stepSize   = max / resolution (1.25)
-- step       = ceiling . (/) $ value stepSize

test1 = putStrLn $ render 8 0 [0..7]
test2 = putStrLn $ render 8 0 [1..8]
test3 = putStrLn $ render 100 0 [0..100]
