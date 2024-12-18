import Advent

import Data.Char
import Data.List

testInput =
  [ "two1nine"
  , "eightwothree"
  , "abcone2threexyz"
  , "xtwone3four"
  , "4nineeightseven2"
  , "zoneight234"
  , "7pqrstsixteen"
  ]

main :: IO ()
main = do
  input <- lines <$> readInput 2023 1
  print (part1 input)
  print (part2 testInput)

toDigits :: [Char] -> [Int]
toDigits = map (read . (:[])) . filter isDigit

getFirstDigit :: String -> Maybe Int
getFirstDigit = 


wordsToDigits :: [Char] -> [Char]
wordsToDigits = \case
  'z':'e':'r':'o':      rest -> '0' : wordsToDigits rest
  'o':'n':'e':          rest -> '1' : wordsToDigits rest
  't':'w':'o':          rest -> '2' : wordsToDigits rest
  't':'h':'r':'e':'e':  rest -> '3' : wordsToDigits rest
  'f':'o':'u':'r':      rest -> '4' : wordsToDigits rest
  'f':'i':'v':'e':      rest -> '5' : wordsToDigits rest
  's':'i':'x':          rest -> '6' : wordsToDigits rest
  's':'e':'v':'e':'n':  rest -> '7' : wordsToDigits rest
  'e':'i':'g':'h':'t':  rest -> '8' : wordsToDigits rest
  'n':'i':'n':'e':      rest -> '9' : wordsToDigits rest
  x:                    rest -> x   : wordsToDigits rest
  []                         -> []

calibration :: [Int] -> Int
calibration digits = 10 * head digits + last digits

part1 = sum . map calibration . map toDigits

part2 = map wordsToDigits
