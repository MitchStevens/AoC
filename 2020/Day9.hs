-- |
module Main where

import Control.Monad
import Data.List

validPairsExist :: [Int] -> Int -> Either ([Int], Int) ()
validPairsExist nums n = if null validPairs then Left (nums, n) else Right ()
  where
    validPairs = do
      a <- sort nums
      b <- sort nums
      guard $ a + b == n
      guard $ a < b

rangeSumsTo :: [Int] -> Int -> [Int]
rangeSumsTo nums n = do
  seq <- filter (\s -> length s >= 2) $ pure nums >>= inits >>= tails
  guard $ sum seq == n
  pure (minimum seq + maximum seq)

part1 :: [Int] -> [Int] -> Maybe ([Int], Int)
part1 nums [] = Nothing
part1 nums (x:xs) =
  either Just (\_ -> part1 (drop 1 (nums <> [x])) xs) (validPairsExist nums x)

main :: IO ()
main = do
  input <- map (read :: String -> Int) . lines <$> readFile "Day9Input.txt"
  let (preamble, rest) = splitAt 25 input
  let Just (nums, invalid) = part1 preamble rest
  print invalid
  print $ head $ rangeSumsTo input invalid
  --print $ part2 preamble rest
