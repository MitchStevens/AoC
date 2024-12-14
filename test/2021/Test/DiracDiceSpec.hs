module Test.DiracDiceSpec where

import Control.Monad
import Control.Monad.State.Lazy
import DiracDice
import Test.Hspec

testGame = newGame 4 8

spec :: Spec
spec = describe "Dirac Dice" $ do
  describe "nzmod" $ do
    it "" $ nzmod 7 5 `shouldBe` 2
    it "" $ nzmod 7 4 `shouldBe` 3
    it "" $ nzmod 7 7 `shouldBe` 7
    it "" $ nzmod 10 10 `shouldBe` 10
  describe "deterministicDie " $ do
    it "" $ evalState (replicateM 3 deterministicDie) testGame `shouldBe` [1, 2, 3]
    it "" $ evalStateT quantumDie testGame `shouldBe` [1, 2, 3]
  describe "takeTurn" $ do
    it "" $ evalState testGame1 testGame `shouldBe` Player 10 10
    it "" $ evalState testGame2 testGame `shouldBe` Player 3 3
    it "" $ evalState testGame3 testGame `shouldBe` Player 14 4
    it "" $ evalState testGame4 testGame `shouldBe` Player 9 6
  describe "runGame" $ do
    it "" $ evalState runDeterministicGame testGame `shouldBe` 739785

dTurn :: (MonadState Game m) => Player -> m Player
dTurn = playerTurn deterministicDie

testGame1 :: (MonadState Game m) => m Player
testGame1 = dTurn (newPlayer 4)

testGame2 :: (MonadState Game m) => m Player
testGame2 = dTurn (newPlayer 4) *> dTurn (newPlayer 8)

testGame3 :: (MonadState Game m) => m Player
testGame3 = do
  p1 <- dTurn (newPlayer 4)
  _ <- dTurn (newPlayer 8)
  dTurn p1

testGame4 :: (MonadState Game m) => m Player
testGame4 = do
  p1 <- dTurn (newPlayer 4)
  p2 <- dTurn (newPlayer 8)
  _ <- dTurn p1
  dTurn p2
