module DiracDice where

import Advent
import Control.Lens
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.State
import Data.Map (Map)

data Player = Player {_score :: Int, _pos :: Int}
  deriving (Eq, Show)

data Game = Game {_player1 :: Player, _player2 :: Player, _numTurns :: Int, _dieRolls :: Int}

makeLenses ''Player
makeLenses ''Game

newPlayer :: Int -> Player
newPlayer = Player 0

newGame :: Int -> Int -> Game
newGame a b = Game (newPlayer a) (newPlayer b) 0 0

-- like mod, only doesn't return 0 when n divides a, returns n
-- nonzero mod
nzmod :: Integral a => a -> a -> a
a `nzmod` n = case a `mod` n of
  0 -> n
  a' -> a'

move :: Int -> Player -> Player
move n (Player score pos) = Player (score + newPos) newPos
  where
    newPos = (pos + n) `nzmod` 10

deterministicDie :: (MonadState Game m) => m Int
deterministicDie = do
  dieRolls += 1
  uses dieRolls (`nzmod` 100)

quantumDie :: StateT Game [] Int
quantumDie = do
  dieRolls += 1
  lift [1 .. 3]

playerTurn :: (MonadState Game m) => m Int -> Player -> m Player
playerTurn die player = do
  rolls <- replicateM 3 die
  numTurns += 1
  pure (move (sum rolls) player)

gameTurn :: (MonadState Game m) => m Int -> m ()
gameTurn die = do
  Game p1 p2 _ rolls <- get
  if even rolls
    then do
      p1' <- playerTurn die p1
      player1 .= p1'
    else do
      p2' <- playerTurn die p2
      player2 .= p2'

runDeterministicGame :: (MonadState Game m) => m Int
runDeterministicGame = do
  Game p1 p2 _ rolls <- get
  if
      | p1 ^. score >= 1000 -> pure (p2 ^. score * rolls)
      | p2 ^. score >= 1000 -> pure (p1 ^. score * rolls)
      | otherwise -> gameTurn deterministicDie *> runDeterministicGame

--runQuantumGame :: StateT Game [] (Map Game Int)
--runQuantumGame = do
--  gameTurn
--  Game p1 p2 <- get
  -- gameTurn quantumDie :: StateT Game [] ()



day21 :: IO ()
day21 = do
  let game = newGame 1 6
  --let (p1, p2) = (newPlayer 4, newPlayer 8)
  print $ evalState runDeterministicGame game
