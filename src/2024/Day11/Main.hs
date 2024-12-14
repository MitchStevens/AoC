{-# LANGUAGE OverloadedStrings #-}

import Advent
import Data.Maybe
import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import Parsers
import Data.Functor.WithIndex
import Data.Foldable

testInput = "125 17"

main = do
  input <- readInput 2024 11
  let Right stones = runParser stonesP "" input
  print (part1 stones)
  print (part2 stones)

type Stones = MultiSet Integer

stonesP :: Parser Stones
stonesP = MS.fromList . map toInteger <$> intP `sepEndBy` space

blink :: Stones -> Stones
blink = MS.concatMap blinkStone
  where
    blinkStone :: Integer -> [Integer]
    blinkStone value
      | value == 0                  = [1]
      | even (length (show value))  = 
        let
          str = show value
          (l, r) = splitAt (length str `div` 2) str
        in [read l, read r]
      | otherwise               = [value*2024]

part1 :: Stones -> Int
part1 = MS.size . (!! 25) . iterate blink

part2 :: Stones -> Int
part2 = MS.size . (!! 75) . iterate blink