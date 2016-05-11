{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Table where

import Data.Monoid
import Data.List
import Data.Text (Text)
import qualified Data.Text as T



-- Types --

newtype Cell a = Cell a deriving (Eq, Show, Functor)
type Row = [Cell Text]
type Table = [Row]
type Column = [Cell Text]



-- Main --

renderTable :: Table -> Text
renderTable = T.unlines . fmap renderRow . sizeColumns
  where
  sizeColumns = mapColumns sizeColumn
  renderRow = T.intercalate columnBorder . fmap cellValue
  columnBorder = " "



-- Helpers --

sizeColumn :: Column -> Column
sizeColumn cells = (fmap . fmap) (T.justifyLeft n ' ') cells
  where
  n = maximum . fmap (T.length . cellValue) $ cells

mapColumns :: (Column -> Column) -> Table -> Table
mapColumns f = transpose . fmap f . transpose

cellValue (Cell x) = x

makeTable :: [[Text]] -> Table
makeTable = fmap . fmap $ Cell



-- Development --

t = putStr $ "\n" ++ T.unpack (renderTable mockData) ++ "\n"

mockData :: Table
mockData = makeTable
  [
    ["", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday"],
    ["Temperature", "3", "5", "10", "11", "9"],
    ["Humidity", "10", "15", "50", "0", "2"],
    ["Cloud Cover", "33", "22", "11", "90", "100"]
  ]
