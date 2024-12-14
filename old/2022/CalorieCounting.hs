module CalorieCounting where

import Advent
import Data.List.Split
import Data.List

day1 :: IO ()
day1 = do
    calories <- (fmap.fmap) read . splitOn [""] <$> readInput 2022 1
    print . maximum . fmap sum $ calories
    print . sum . take 3 . reverse . sort . fmap sum $ calories