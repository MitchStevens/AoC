{-# LANGUAGE TemplateHaskell #-}
import Control.Lens
import Control.Monad.ST
import Control.Monad.State.Strict
import Data.HashTable.ST.Basic as M

-- given an int, return the
type History = Either Int (Int, Int)
data Game s = Game
  { _turn :: Int
  , _history :: HashTable s Int History
  , _lastSpoken :: Int
  } deriving Show
makeLenses ''Game

updateHistory :: Int -> StateT (Game s) (ST s) ()
updateHistory n game = do
  index <- gets _turn
  history %= (M.mutate (((, ()) . Just . f index) n)
  where
    f :: Int -> Maybe History -> History
    f index = \case
      Nothing                     -> Left index
      Just (Left prevIndex)       -> Right (index, prevIndex)
      Just (Right (prevIndex, _)) -> Right (index, prevIndex)

startWith :: Int -> StateT (Game s) (ST s) ()
startWith n game = do
  updateHistory n
  lastSpoken .= n
  turn += 1

nextTurn :: StateT (Game s) (ST s) ()
nextTurn = do
  last <- use lastSpoken
  h <- use (history . to (! last)) -- (history . ix last))
  let next = case h of {
    Left _ -> 0;
    Right (p, q) -> p - q
  }
  updateHistory next
  lastSpoken .= next
  turn += 1

runGame :: [Int] -> Int -> ST s Game
runGame initial numTurns = do
  history <- new
  execStateT (do
    traverse startWith initial
    replicateM_ (numTurns - (length initial)) nextTurn)
    history

main :: IO ()
main = do
  let puzzleInput = [8,13,1,0,18,9]
  let example = [0, 3, 6]
  let prob1 = runGame puzzleInput 2020
  let prob2 = runGame puzzleInput 300000
  print . _lastSpoken $ prob1
  print . _lastSpoken $ runGame puzzleInput 300000
  print . _lastSpoken $ runGame puzzleInput 400000
  print . _lastSpoken $ runGame puzzleInput 500000
  print . _lastSpoken $ runGame puzzleInput 600000
  print . _lastSpoken $ runGame puzzleInput 700000
  print . _lastSpoken $ runGame puzzleInput 800000
  print . _lastSpoken $ runGame puzzleInput 900000
