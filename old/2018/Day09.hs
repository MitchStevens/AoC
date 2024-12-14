{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
import Text.Parsec (Parsec, string, letter, sepBy, newline, parse)
import Data.Char (ord)
import Data.List hiding (insert)
import Data.Monoid
import Data.Ord
import Data.Array
import Data.Traversable (for)
import Control.Monad.State.Strict
import Data.Foldable (traverse_, for_)

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Control.Lens

type Parser a = Parsec String () a

data Zipper a = Zipper [a] a [a]

instance Show (Zipper Int) where
  show (Zipper ls v rs) = show $ drop n l <> take n l
    where
      l = reverse ls <> (v : rs)
      Just n = elemIndex 0 l 

data GameState = Game
  { _numPlayers :: Int
  , _currentPlayer :: Int
  , _scores :: Map Int Int
  , _board :: Zipper Int
  , _marbles :: [Int]
  } deriving Show
makeLenses ''GameState

gameState :: Int -> Int -> GameState
gameState n last = Game
  { _numPlayers = n
  , _currentPlayer = 0
  , _scores = M.empty
  , _board = Zipper [] 0 []
  , _marbles = [1..last]
  }

game = gameState 473 70904
game2 = gameState 473 (100*70904)

main :: IO ()
main = do
  print(winningElf bigboi)
  print(winningElf bigboi2)
  -- print $ winningElf game
  -- print $ winningElf game2

winningElf :: GameState -> (Int, Int)
winningElf gs = maximumBy (comparing snd) $ M.assocs scores
  where scores = _scores $ execState runGame gs

left :: Zipper a -> Zipper a
left (Zipper (l:ls) v rs) = Zipper ls l (v:rs)
left (Zipper [] v rs) = Zipper (tail rs') (head rs')  []
  where rs' = reverse (v:rs)

right :: Zipper a -> Zipper a
right (Zipper ls v (r:rs)) = Zipper (v:ls) r rs
right (Zipper ls v []) = Zipper [] (head ls') (tail ls')
  where ls' = reverse (v:ls)

move :: Either Int Int -> Zipper a -> Zipper a
move dir zipper = case dir of
  Left  n -> iterate left  zipper !! n
  Right n -> iterate right zipper !! n

insert :: a -> Zipper a -> Zipper a
insert a (Zipper ls v rs) = Zipper (ls) a (v:rs)

remove :: Zipper a -> Zipper a
remove (Zipper ls v (r:rs)) = Zipper ls r rs
remove (Zipper ls v []) = Zipper [] (head ls') (tail ls')
  where ls' = reverse ls

newMarble :: State GameState Int
newMarble = do
  marble <- uses marbles head
  marbles %= tail
  pure marble

takeMarble :: State GameState Int
takeMarble = do
  marble <- uses board (\(Zipper ls v rs) -> v)
  board %= remove
  pure marble

placeMarble :: State GameState ()
placeMarble = do
  marble <- newMarble
  if (marble `mod` 23 == 0)
    then do
      curr <- use currentPlayer
      board %= move (Left 7)
      marble2 <- takeMarble
      scores %= M.insertWith (+) curr (marble + marble2)
    else do
      board %= move (Right 2)
      board %= insert marble

runGame :: State GameState ()
runGame = do
  isFinished <- uses marbles null
  unless isFinished $ do
    placeMarble
    n <- use numPlayers
    currentPlayer %= (\curr -> (curr + 1) `mod` n)
    runGame