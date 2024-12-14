{-# LANGUAGE TemplateHaskell #-}
import Data.IntMap.Strict (IntMap, (!))
import qualified Data.IntMap.Strict as M
import Control.Monad.State
import Control.Lens

-- given an int, return the
type History = Either Int (Int, Int)
data Game = Game
  { _turn :: Int
  , _history :: IntMap History
  , _lastSpoken :: Int
  } deriving Show
makeLenses ''Game

updateHistory :: Int -> State Game ()
updateHistory n = do
  index <- gets _turn
  history %= (M.alter (f index) n)
  where
    f :: Int -> Maybe History -> Maybe History
    f index Nothing = Just (Left index)
    f index (Just (Left prevIndex)) = Just (Right (index, prevIndex))
    f index (Just (Right (prevIndex, _))) = Just (Right (index, prevIndex))

startWith :: Int -> State Game ()
startWith n = do
  updateHistory n
  lastSpoken .= n
  turn += 1

nextTurn :: State Game ()
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

runGame :: [Int] -> Int -> Game
runGame initial numTurns = execState
        ( do
            forM_ initial startWith
            replicateM_ (numTurns - (length initial)) nextTurn)
        (Game 0 M.empty undefined)

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
