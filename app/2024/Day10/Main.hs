{-# LANGUAGE OverloadedStrings #-}

import Advent
import Algebra.Graph.AdjacencyMap
import Data.Array (Array, (!))
import qualified Data.Array as A
import Array2D
import Point
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import qualified Algebra.Graph.Acyclic.AdjacencyMap as Acyclic
import Parsers
import Data.Foldable.WithIndex
import Data.Foldable

testInput =
  [ "89010123"
  , "78121874"
  , "87430965"
  , "96549874"
  , "45678903"
  , "32019012"
  , "01329801"
  , "10456732"
  ]


main = do
  input <- lines <$> readInput 2024 10
  let trailMap = readArray2D (read . (:[])) input
  print (part1 trailMap)
  print (part2 trailMap)

getTrailHeads :: Array Point Int -> [Point]
getTrailHeads array2D = ifoldMap (\p n -> if n == 0 then [p] else []) array2D

validSteps :: Array Point Int -> Point -> [Point]
validSteps array2D loc =
  filter (\l -> safeLookup array2D l == Just (currentHeight + 1)) (cardinal loc)
  where
    currentHeight = array2D ! loc

countHeads :: Array Point Int -> Array Point (Set Point)
countHeads trailMap = recurse trailMap f
  where
    f arr loc = 
      if trailMap ! loc == 9 
        then S.singleton loc
        else S.unions ((arr !) <$> validSteps trailMap loc)

countRoutes :: Array Point Int -> Array Point Int
countRoutes trailMap = recurse trailMap f
  where
    f arr loc = 
      if trailMap ! loc == 9 
        then 1
        else sum ((arr !) <$> validSteps trailMap loc)


part1 :: Array Point Int -> Int
part1 trailMap = sum . map S.size $ (heads !) <$> getTrailHeads trailMap
  where
    heads = countHeads trailMap

part2 :: Array Point Int -> Int
part2 trailMap = sum $ (routes !) <$> getTrailHeads trailMap
  where
    routes = countRoutes trailMap