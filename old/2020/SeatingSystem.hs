module SeatingSystem where

import Advent
import Array2D
import Point
import Data.Array
import Data.Foldable
import Data.Maybe

data Seat = Floor | Empty | Occupied
    deriving Eq

instance Show Seat where
    show = \case
        Floor -> "."
        Empty -> "L"
        Occupied -> "#"

readSeatLayout :: [String] -> Array Point Seat
readSeatLayout = readArray2D $ \case
    'L' -> Empty
    '#' -> Occupied
    _ -> Floor

nextState :: Array Point Seat -> Point -> Seat
nextState arr point = case arr ! point of
    Floor -> Floor
    Empty -> if numAdjOccupied == 0 then Occupied else Empty
    Occupied -> if numAdjOccupied >= 4 then Empty else Occupied
    where numAdjOccupied = length . filter (==Occupied) . mapMaybe (safeLookup arr) $ adjacent point
    
firstSeen :: Array Point Seat -> Direction -> Point -> Maybe Seat
firstSeen arr direction point = do
    seat <- safeLookup arr (direction point)
    if seat == Floor
        then firstSeen arr direction (direction point)
        else Just seat

tolerantNextState :: Array Point Seat -> Point -> Seat
tolerantNextState arr point = case arr ! point of
    Floor -> Floor
    Empty -> if numAdjOccupied == 0 then Occupied else Empty
    Occupied -> if numAdjOccupied >= 5 then Empty else Occupied
    where numAdjOccupied = length . filter (==Occupied) . mapMaybe (\d -> firstSeen arr d point) $ adjacentDirections

countOccupied :: Array Point Seat -> Int
countOccupied = length . filter (Occupied==) . elems

testInput = 
    [ "L.LL.LL.LL"
    , "LLLLLLL.LL"
    , "L.L.L..L.."
    , "LLLL.LL.LL"
    , "L.LL.LL.LL"
    , "L.LLLLL.LL"
    , "..L.L....."
    , "LLLLLLLLLL"
    , "L.LLLLLL.L"
    , "L.LLLLL.LL"
    ]

day11 :: IO ()
day11 = do
    input <- readInput 2020 11
    --let input = testInput
    let seatLayout = readSeatLayout input
    --putStrLn (showArray2D seatLayout)
    let (count, steady) = steadyState (extendArray2D nextState) seatLayout
    print (countOccupied steady)
    let (count, steady) = steadyState (extendArray2D tolerantNextState) seatLayout
    print (countOccupied steady)
