module Bingo where

import Prelude hiding (filter)
import Advent
import Data.Functor.Compose
import Data.Witherable
import Control.Lens
import Data.List hiding (filter)
import Data.List.Split
import Data.Maybe
import Control.Monad

type Bingo = (Compose (Compose [] []) Maybe) Int

readBingo :: [String] -> Bingo
readBingo strs = bingo $ fmap read . words <$> strs

bingo :: [[Int]] -> Bingo
bingo = Compose . Compose . (fmap.fmap) Just

indexBingo :: Bingo ->  (Int, Int) -> Maybe Int
indexBingo (Compose (Compose b)) (i, j) = (b !! i) !! j

callNumber :: Int -> Bingo -> Bingo
callNumber n = filter (/=n)

horizontals, verticals  :: [[(Int, Int)]]
horizontals = [ [(i,j) | j <- [0..4]] | i <- [0..4] ]
verticals = transpose horizontals

isWon :: Bingo -> Bool
isWon bingo = any (allMarked bingo) (verticals <> horizontals)
  where
    allMarked :: Bingo -> [(Int, Int)] -> Bool
    allMarked bingo = all (isNothing . indexBingo bingo)

score :: Int -> Bingo -> Int
score lastCalled bingo = sum bingo * lastCalled


type BingoGame = [Bingo]

callNumberGame :: Int -> BingoGame -> Either (Int, Bingo) BingoGame
callNumberGame n game = maybe (Right game') (Left . (n,)) (find isWon game')
  where game' = callNumber n <$> game

getLoser :: Int -> BingoGame -> Either (Int, Bingo) BingoGame
getLoser n game = case game' of
  [lastPlayer] -> if isWon lastPlayer then Left (n, lastPlayer) else Right game'
  _ -> Right (filter (not.isWon) game')
  where game' = callNumber n <$> game

testInput = [7,4,9,5,11,17,23,2,0,14,21,24] --,24

testBingo = readBingo
  [ "14 21 17 24  4"
  , "10 16 15  9 19"
  , "18  8 23 26 20"
  , "22 11 13  6  5"
  , " 2  0 12  3  7"
  ]

day4 :: IO ()
day4 = do
  input <- readInput 2021 4
  let numbers = (read :: String -> [Int]) ("[" <> head input <> "]")
  let bingoGame = readBingo <$> splitWhen (=="") (drop 2 input)
  print $ either (uncurry score) (error "failed!") $
    foldM (flip callNumberGame) bingoGame numbers
  print $ either (uncurry score) (error "failed!") $
    foldM (flip getLoser) bingoGame numbers