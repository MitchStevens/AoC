{-# LANGUAGE OverloadedStrings #-}

import Advent
import qualified Data.Array as A
import Data.List
import Control.Monad
import Array2D
import Point
import Data.Maybe
import Data.Ix
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

testInput = 
  [ "............"
  , "........0..."
  , ".....0......"
  , ".......0...."
  , "....0......."
  , "......A....."
  , "............"
  , "............"
  , "........A..."
  , ".........A.."
  , "............"
  , "............"
  ]

main = do
  input <- lines <$> readInput 2024 8
  let array2D = readArray2D id input
  let nodes = M.delete '.' (groupArray2D array2D)
  print (show nodes)
  let a = S.fromList [(6,5),(8,8),(9,9)]
  print (show (findAntinodes a))
  print (show (part1 nodes (inRange (A.bounds array2D))))
  print (show (part2 nodes (inRange (A.bounds array2D))))

findAntinodes :: Set Point -> Set Point
findAntinodes points = S.fromList $ do
  a <- S.toList points
  b <- S.toList points
  guard (a /= b)
  [ a + a - b, b + b - a ]

findTrueAntinodes :: (Point -> Bool) -> Set Point -> Set Point
findTrueAntinodes inBounds points = S.fromList $ do
  a <- S.toList points
  b <- S.toList points
  guard (a /= b)
  let diff = a - b

  let as = takeWhile inBounds $ iterate (+ diff) (b+diff)
  let bs = takeWhile inBounds $ iterate (\x -> x - diff) (a-diff)
  as <> bs


part1 :: Map Char (Set Point) -> (Point -> Bool) -> Int
part1 nodes inBounds = S.size . S.filter inBounds $ foldMap findAntinodes nodes
  
  
part2 :: Map Char (Set Point) -> (Point -> Bool) -> Int
part2 nodes inBounds = S.size . S.filter inBounds $ foldMap (findTrueAntinodes inBounds) nodes
--part2 :: [Equation] -> Int
--part2 = sum . map total . filter isPart2Solvable