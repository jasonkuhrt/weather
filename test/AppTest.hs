module AppTest where

import Test.Hspec
import qualified SparklineTest

{-# ANN module "HLint: ignore Redundant do" #-}



main :: IO ()
main = hspec $ do
  SparklineTest.test
