{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module RockPaperScissors where

import Advent

data Move = Rock | Paper | Scissors
    deriving Eq

part1, part2 :: String -> (Move, Move)
part1 [a, ' ', b] = (you, me)
    where
        you = case a of { 'A' -> Rock; 'B' -> Paper; 'C' -> Scissors }
        me  = case b of { 'X' -> Rock; 'Y' -> Paper; 'Z' -> Scissors }
part2 [a, ' ', b] = (you, me)
    where
        you = case a of { 'A' -> Rock; 'B' -> Paper; 'C' -> Scissors }
        me  = case b of { 'X' -> losesAgainst you; 'Y' -> you; 'Z' -> winsAgainst you }

winsAgainst, losesAgainst :: Move -> Move
winsAgainst = \case
    Rock     -> Paper
    Paper    -> Scissors
    Scissors -> Rock
losesAgainst = winsAgainst . winsAgainst

beats :: Move -> Move -> Bool
beats a b = a == winsAgainst b

playRound :: Move -> Move -> Int
playRound you me
    | me `beats` you = score + 6
    | you `beats` me = score + 0
    | otherwise      = score + 3
    where score = case me of { Rock -> 1; Paper -> 2; Scissors -> 3 }

day2 :: IO ()
day2 = do
    input <- readInput 2022 2
    print . sum . map (uncurry playRound . part1) $ input
    print . sum . map (uncurry playRound . part2) $ input
