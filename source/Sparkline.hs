-- Note that annother implementation can be found at:
-- https://rosettacode.org/wiki/Sparkline_in_unicode#Haskell

module Sparkline (
  draw,
  drawWithRange
) where

import Data.Char
import Data.Text (Text)
import qualified Data.Text as Text

{-# ANN module "HLint: ignore Eta reduce" #-}



-- API --

draw :: RealFrac a => [a] -> Text
draw xs = drawWithRange (calcRange xs) xs


drawWithRange :: RealFrac a => (a,a) -> [a] -> Text
drawWithRange range = Text.pack . fmap (drawDataMeasureWithRange range)


-- Note: This function has a lexical dependency on `dataMeasures`
--
-- Given the datum's percentage-placement within the data range
-- we can select the closest matching DM to that percent.
-- The reason for "closest match" is that we only have 8 DMs to work with
-- via limit of suitable unicode characters: Remember, we are
-- outputting to terminal, not an arbitrary bitmap! This is the
-- "resolution" of the sparkline.
drawDataMeasureWithRange :: (Eq a, RealFrac a) => (a,a) -> a -> Char
drawDataMeasureWithRange range x =
  selectAtPercent dataMeasures (percentage range x)


dataMeasures :: String
dataMeasures = map chr [0x2581..0x2588]



-- Helpers --

calcRange :: (Ord a, Num a) => [a] -> (a,a)
calcRange xs = (minimum xs, maximum xs)

percentage :: (RealFrac a) => (a,a) -> a -> a
percentage (bot,top) xAbs = p
  where
  p     = xRel / r
  xRel  = xAbs - bot
  r     = top - bot

-- `a` must be within inclusive range of 0-1
selectAtPercent :: RealFrac a => [b] -> a -> b
selectAtPercent xs p =
  -- Coercion because `length` returns Int but `round` wants `RealFrac a`
  -- TODO Benchmark use of `length` here versus pulling it out in e.g.
  --      `drawWithRange`
  xs !! round (realToFrac (length xs - 1) * p)
