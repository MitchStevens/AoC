{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module CampCleanup where

import Range
import Advent (readInput)
import Data.List.Split
import Data.Ix


parseRanges :: String -> (Range Int, Range Int)
parseRanges str = let [range1, range2] = parseRange <$> splitOn "," str in (range1, range2)
    where parseRange s = let [a, b] = splitOn "-" s in toRange (read a) (read b)

fullyContains :: ( Ix a) =>Range a -> Range a -> Bool
fullyContains r1 r2 = r1 `isWithin` r2 || r2 `isWithin` r1
    
testInput =   
    [ "2-4,6-8"
    , "2-3,4-5"
    , "5-7,7-9"
    , "2-8,3-7"
    , "6-6,4-6"
    , "2-6,4-8"
    ]

day4 :: IO ()
day4 = do
    input <- readInput 2022 4 
    --let input = testInput
    let ranges = map parseRanges input
    print . length . filter (uncurry fullyContains) $ ranges
    print . length . filter (uncurry overlaps) $ ranges
