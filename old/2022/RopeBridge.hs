{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module RopeBridge where

import Advent
import Point
import Data.Foldable
import Control.Lens
import Data.List
import Data.Function
import Data.Char (intToDigit)
import Data.Maybe

type Snake = [Point]

moveSnake :: (Point -> Point) -> Snake -> Snake
moveSnake dir = slinkTowardsHead . moveHead
    where
        moveHead = ix 0 %~ dir

        closestCardinal :: Point -> Point -> Point
        closestCardinal target p = minimumBy (compare `on` adjDist p) (cardinal target)

        slinkTowardsHead :: Snake -> Snake
        slinkTowardsHead (a:b:ss) = a : if adjDist a b > 1 then slinkTowardsHead (closestCardinal a b:ss) else b:ss
        slinkTowardsHead snake = snake

parseMovements :: [String] -> [Point -> Point]
parseMovements lines = do
    line <- lines
    let [c, count] = words line
    let direction = case c of {
        "U" -> down;
        "D" -> up;
        "L" -> left;
        "R" -> right;
        _ -> error "wow!"
    }
    replicate (read count) direction

testInput =
    [ "R 4"
    , "U 4"
    , "L 3"
    , "D 1"
    , "R 4"
    , "D 1"
    , "L 5"
    , "R 2"
    ]

testInput2 = 
    [ "R 5"
    , "U 8"
    , "L 8"
    , "D 3"
    , "R 17"
    , "D 10"
    , "L 25"
    , "U 20"
    ]

printSnake :: Snake -> IO ()
printSnake snake = traverse_ putStrLn . (map.map) f $ [ (,y) <$> [0..10] | y <- reverse [0..10] ]
    where
        f :: Point -> Char
        f (0, 0) = 's'
        f p = fromMaybe '.' $ do
            i <- elemIndex p snake
            pure (if i == 0 then 'h' else intToDigit i)

day9 :: IO ()
day9 = do
    let input = testInput
    --input <- readInput 2022 9
    let movements = parseMovements input
    let littleSnake = replicate 2 (0, 0)
    print . length . nub . map last $ scanl (flip moveSnake) littleSnake movements
    let bigSnake = replicate 10 (0, 0)
    print . length . nub . map last $ scanl (flip moveSnake) bigSnake movements
    traverse_ printSnake $ scanl (flip moveSnake) bigSnake movements
