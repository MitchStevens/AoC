module CrabCombat where

import Advent
import Data.Bifunctor
import Data.Sequence (Seq(..))
import qualified Data.Sequence as S

combat :: Seq Int -> Seq Int -> Int
combat Empty cards = score cards
combat cards Empty = score cards
combat (c1 :<| cs1) (c2 :<| cs2) = 
    if c1 > c2 
        then combat (cs1 :|> c1 :|> c2) cs2
        else combat cs1 (cs2 :|> c2 :|> c1)


score :: Seq Int -> Int
score cards = sum (S.zipWith (*) (S.reverse cards) (S.fromList [1..(S.length cards)]))

testInput = 
    [ "Player 1:"
    , "9"
    , "2"
    , "6"
    , "3"
    , "1"
    , ""
    , "Player 2:"
    , "5"
    , "8"
    , "4"
    , "7"
    , "10"
    ]

parseInput :: [String] -> (Seq Int, Seq Int)
parseInput strs = (parseRaw p1Raw, parseRaw p2Raw)
    where
        (p1Raw, p2Raw) = second tail $ span (/="") strs
        parseRaw = S.fromList . map read . tail

day22 :: IO ()
day22 = do
    --let input = testInput
    input <- readInput 2020 22
    let (p1, p2) = parseInput input
    print $ combat p1 p2