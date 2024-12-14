{-# LANGUAGE OverloadedStrings #-}

import Advent
import Data.Array (Array)
import Data.List
import Control.Monad
import Array2D
import Point
import Data.Maybe

main = do
  let
    testInput =
      [ "MMMSXXMASM"
      , "MSAMXMSMSA"
      , "AMXSXMAAMM"
      , "MSAMASMSMX"
      , "XMASAMXAMM"
      , "XXAMMXXAMA"
      , "SMSMSASXSS"
      , "SAXAMASAAA"
      , "MAMMMXMMMM"
      , "MXMXAXMASX"
      ]

  input <- lines <$> readInput 2024 4
  let array2d = readArray2D id input
  print (show (part1 array2d))
  print (show (part2 array2d))

countXmasStartingAt :: Array Point Char -> Point -> Int
countXmasStartingAt array2D loc = sum $ flip fmap adjacentDirections $ \dir ->
  if take 4 (look loc dir array2D) == "XMAS" then 1 else 0

hasCrossStartingAt :: Array Point Char -> Point -> Bool
hasCrossStartingAt array2d loc = fromMaybe False $ do
  center <- safeLookup array2d loc
  ms <- traverse (safeLookup array2d) (sequence diagonals loc)
  guard (center == 'A')
  guard (sort ms == "MMSS" && ms !! 0 /= ms !! 3)
  pure True

part1 :: Array Point Char -> Int
part1 = sum . extendArray2D countXmasStartingAt

part2 :: Array Point Char -> Int
part2 = sum . fmap (\b -> if b then 1 else 0) . extendArray2D hasCrossStartingAt