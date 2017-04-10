module Main where

import Test.Hspec
import Lib


main :: IO ()
main = hspec $ do
  describe "elongase" .
      it "should add two carbons to carboxylic end" $
        elongase ala `shouldBe` FA 20 [11, 14, 17]
  describe "betaOxidation" .
      it "should remove two carbons from carboxylic end" $
        betaOxidation ala `shouldBe` FA 16 [7, 10, 13]
  describe "delta5Desaturase" .
      it "should add double bond to delta-5 position" $
        delta5Desaturase ala `shouldBe` FA 18 [5, 9, 12, 15]
  describe "delta6Desaturase" .
      it "should add double bond to delta-6 position" $
        delta6Desaturase ala `shouldBe` FA 18 [6, 9, 12, 15]
  describe "findPathways" .
      it "should find biochemical pathway for the synthesis of DHA" $
        findPathways enzymes dha ala `shouldBe`
        [
        [ FA 18 [9,12,15]
        , FA 18 [6,9,12,15]
        , FA 20 [8,11,14,17]
        , FA 20 [5,8,11,14,17]
        , FA 22 [7,10,13,16,19]
        , FA 24 [9,12,15,18,21]
        , FA 24 [6,9,12,15,18,21]
        , FA 22 [4,7,10,13,16,19]
        ]
        ]
