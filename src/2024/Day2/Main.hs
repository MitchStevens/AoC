{-# LANGUAGE OverloadedStrings #-}

import Advent
import Data.List
import Data.Bifunctor
import Data.Maybe
import Control.Monad
import Control.Comonad
import Zipper

newtype Delta = Delta Int

main = do
  input <- readInput 2024 2
  let levels = fmap (toZipper . map read . words) . lines $ input
  print (show (part1 levels))
  print (show (part2 levels))
  print . show . fmap (and .  extend isSafeWithDampener) $ levels

toDeltas :: [Int] -> [Delta]
toDeltas levels = zipWith (\a b -> Delta (a - b)) levels (tail levels)

isSmallDiff :: Int -> Int -> Bool
isSmallDiff a b = abs (a-b) >= 1 && abs (a-b) <= 3

isSafe :: Zipper Int -> Bool
isSafe (Zipper (l:ls) v (r:rs)) = 
  signum (l - v) == signum (v - r) && isSmallDiff l v && isSmallDiff v r
isSafe (Zipper [] v (r:rs)) = isSmallDiff v r
isSafe (Zipper (l:ls) v []) = isSmallDiff l v
isSafe (Zipper [] _ []) = True

isSafeWithDampener :: Zipper Int -> Bool
isSafeWithDampener level@(Zipper (l:ls) v rs) =
  isSafe level || isSafe (Zipper ls l rs)
isSafeWithDampener level@(Zipper [] v (r:rs)) =
  isSafe level || isSafe (Zipper [] r rs)
isSafeWithDampener level@(Zipper [] v []) =
  isSafe level
  
part1 :: [Zipper Int] -> Int
part1 = length . filter and . fmap (extend isSafe)

part2 :: [Zipper Int] -> Int
part2 = length . filter and . fmap (extend isSafeWithDampener)