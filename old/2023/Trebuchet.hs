module Trebuchet where

import Advent
import Data.Char

day1 :: IO ()
day1 = do
  lines <- readInput 2023 1
  print $ sum (map calibration lines)

calibration :: [Char] -> Int
calibration text = read [ head digits, last digits ]
  where digits = filter isDigit text

calibration2 :: [Char] -> Int
calibration2 text
