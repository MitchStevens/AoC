module HydrothermalVenture where

import Data.Map (Map)
import qualified Data.Map as M
import Parsers
import Point
import Text.ParserCombinators.ReadP (char, string, readP_to_S)
import Advent (readInput)

data Line = Line Point Point
    deriving (Show)

instance Read Line where
    readsPrec _ = readP_to_S $ do
        p1 <- (,) <$> intP <* char ',' <*> intP
        string " -> "
        p2 <- (,) <$> intP <* char ',' <*> intP
        pure $ Line p1 p2

isDiagonal :: Line -> Bool
isDiagonal (Line (x1, y1) (x2, y2)) = x1 /= x2 && y1 /= y2

toPoints :: Line -> [Point]
toPoints (Line (x1, y1) (x2, y2)) = zip (absRange x1 x2) (absRange y1 y2)
    where 
        absRange a b = case compare a b of
            LT -> [a..b]
            EQ -> repeat a
            GT -> reverse [b..a]

toDiagram :: [Line] -> Map Point Int
toDiagram = M.unionsWith (+) . map (M.fromList . map (,1) . toPoints)

testLines = 
    [ "0,9 -> 5,9"
    , "8,0 -> 0,8"
    , "9,4 -> 3,4"
    , "2,2 -> 2,1"
    , "7,0 -> 7,4"
    , "6,4 -> 2,0"
    , "0,9 -> 2,9"
    , "3,4 -> 1,4"
    , "0,0 -> 8,8"
    , "5,5 -> 8,2"
    ]

day5 :: IO ()
day5 = do
    --let input = testLines
    input <- readInput 2021 5
    print . length . M.filter (>1). toDiagram . filter (not.isDiagonal) . map read $ input
    print . length . M.filter (>1). toDiagram . map read $ input