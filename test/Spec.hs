module Main where

import Robot
import Control.Monad.State

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)

main :: IO ()
main = hspec $ do
  describe "Robot" $ do
    it "(place) should give False if outside a tabletop" $ do
      p (snd (runState (place 7 7 North) robot)) `shouldBe` False

    it "(place) should give True if inside a tabletop" $ do
      p (snd (runState (place 0 0 North) robot)) `shouldBe` True

    it "(left) turn should change from North to West if a robot is placed" $ do
      d (snd (runState (do place 0 0 North ; left) robot)) `shouldBe` West

    it "(left) turn should NOT change from North to West if a robot is NOT placed" $ do
      d (snd (runState (do place 7 7 North ; left) robot)) `shouldBe` North

    it "(right) turn should change from North to East if a robot is placed" $ do
      d (snd (runState (do place 0 0 North ; right) robot)) `shouldBe` East

    it "(right) turn should NOT change from North to East if a robot is NOT placed" $ do
      d (snd (runState (do place 7 7 North ; right) robot)) `shouldBe` North

    it "(move) should be ignored if a robot is NOT placed" $ do
      y (snd (runState (do place 7 7 North ; move) robot)) `shouldBe` 0

    it "(move) to East should increase X" $ do
      x (snd (runState (do place 0 0 East ; move) robot)) `shouldBe` 1

    it "(move) to West should decrease X" $ do
      x (snd (runState (do place 1 0 West ; move) robot)) `shouldBe` 0

    it "(move) to East should NOT result in falling of a tabletop if X > 5" $ do
      x (snd (runState (do place 5 0 East ; move) robot)) `shouldBe` 5

    it "(move) to West should NOT result in falling of a tabletop if X < 0" $ do
      x (snd (runState (do place 0 0 West ; move) robot)) `shouldBe` 0

    it "(move) to North should increase Y" $ do
      y (snd (runState (do place 0 0 North ; move) robot)) `shouldBe` 1

    it "(move) to South should decrease Y" $ do
      y (snd (runState (do place 0 1 South ; move) robot)) `shouldBe` 0

    it "(move) to North should NOT result in falling of a tabletop if Y > 5" $ do
      y (snd (runState (do place 0 5 North ; move) robot)) `shouldBe` 5

    it "(move) to South should NOT result in falling of a tabletop if Y < 0" $ do
      y (snd (runState (do place 0 0 South ; move) robot)) `shouldBe` 0

    it "(move) number six should be ignored, but the next command should be accepted" $ do
      snd (runState (do place 0 0 North ; move ; move ; move ; move ; move ; move ; right) robot) `shouldBe` Robot True 0 5 East