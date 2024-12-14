module TreetopTreeHouse where

import Data.Array
import Point
import qualified Data.Array as A
import Data.Array (Array, (!))
import Data.List
import qualified Data.Set as S
import Data.Set (Set)
import Data.Char (digitToInt)
import Advent (readInput)
import Array2D (look, readArray2D)

type Tree = Point
type Forest = Array Point Int

-- like takeWhile, but also returns the first failing element
takeWhileWithFailing :: (a -> Bool) -> [a] -> [a]
takeWhileWithFailing f = \case
    x:xs -> if f x then x : takeWhileWithFailing f xs else [x]
    [] -> []

view :: Tree -> Forest -> Direction -> [Int]
view tree forest direction = tail (look tree direction forest)

isVisible :: Tree -> Forest -> Bool
isVisible tree forest = any (\d -> foldr max (-1) (view tree forest d) < forest ! tree) cardinalDirections

scenicScore :: Tree -> Forest -> Int
scenicScore tree forest = product $ map scenicView cardinalDirections
    where scenicView direction = length . takeWhileWithFailing ((forest ! tree) >) $ view tree forest direction

testInput =
    [ "30373"
    , "25512"
    , "65332"
    , "33549"
    , "35390"
    ]

day8 :: IO ()
day8 = do
    --let input = testInput
    input <- readInput 2022 8
    let forest = readArray2D digitToInt input
    print . length $ filter (\t -> isVisible t forest) (A.indices forest)
    print . maximum $ map (\t -> scenicScore t forest) (A.indices forest)
    --print $ map (\t -> scenicScore t forest) [(2, 1), (2, 3), (1, 1)]
    let tree = (2, 1)
    let scenicView direction = takeWhileWithFailing ((forest ! tree) >) $ view tree forest direction

    print (forest ! tree)
    print $ map scenicView cardinalDirections