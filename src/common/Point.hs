module Point where

import Data.Ix

type Point = (Int, Int)
type Direction = Point -> Point

instance Num Point where
  (x1, y1) + (x2, y2) = (x1+x2, y1+y2)
  (x1, y1) - (x2, y2) = (x1-x2, y1-y2)
  (x1, y1) * (x2, y2) = (x1*x2, y1*y2)
  --fromInteger n =  (fromIntegral n, fromIntegral n)


origin :: Point
origin = (0, 0)

--allPoints :: [Point]
--allPoints = origin : ([1..] >>= (\x -> singleCorners x >>= allRotations))
--    where
--        allRotations = take 4 . iterate (\(x, y) -> (y, -x))
--        singleCorners n = range ((0, n), (n-1, n)) <> range ((n, n), (n, 1))

taxicab :: Point -> Point -> Int
taxicab (x1, y1) (x2, y2) = abs (x1-x2) + abs (y1-y2)

adjDist :: Point -> Point -> Int
adjDist (x1, y1) (x2, y2) = abs (x1-x2) `max` abs (y1-y2)

surrounding :: Point -> [Point]
--surrounding (x, y) = (,) <$> [(x-1)..(x+1)] <*> [(y-1)..(y+1)]
surrounding  = sequence (id : adjacentDirections)

adjacent :: Point -> [Point]
adjacent = sequence adjacentDirections

cardinal :: Point -> [Point]
cardinal = sequence cardinalDirections

left, right, up, down :: Point -> Point 
left (x, y) = (x-1, y)
right (x, y) = (x+1, y)
up (x, y) = (x, y-1)
down (x, y) = (x, y+1)

cardinalDirections :: [Direction]
cardinalDirections = [up, right, down, left]

diagonals :: [Direction]
diagonals = [ v . h | v <- [up, down], h <-[right, left]]

adjacentDirections :: [Direction]
adjacentDirections = cardinalDirections <> diagonals