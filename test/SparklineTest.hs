{-# LANGUAGE OverloadedStrings #-}

module SparklineTest (test) where

import Test.Hspec
import Sparkline



test :: Spec
test = describe "Sparkline" $ do



  describe "drawWithRange" $ do

    it "[1..8] = ▁▂▃▄▅▆▇█" $ do
      let viz = drawWithRange (1,8) [1..8]
      shouldBe "▁▂▃▄▅▆▇█" viz

    it "works with negative numbers" $ do
      let viz = drawWithRange (-1000,1000) [-1000,100,1000,500,200,-400,-700,621,-189,3]
      shouldBe "▁▅█▆▅▃▂▇▄▅" viz



  describe "draw" $ do

    it "infers range from min/max in data" $ do
      let viz = draw [1..8]
      shouldBe "▁▂▃▄▅▆▇█" viz


    it "[0,1] = ▁█" $ do
      let viz = draw [0,1]
      shouldBe "▁█" viz
