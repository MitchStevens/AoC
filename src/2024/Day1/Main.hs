{-# LANGUAGE OverloadedStrings #-}

import Advent
import Data.List
import Data.Bifunctor
import Data.Maybe

main = do
  input <- readInput 2024 1
  let ids = fmap parseLine . lines $ input
  print "Hello World!"
  print . show $ (map (abs . uncurry (-)) $ ids)
  print (show (part1 ids))
  print (show (part2 ids))

parseLine :: String -> (Int, Int)
parseLine line = case words line of
  [ a, b ] -> (read a, read b)
  _ -> undefined

-- hell yeah
part1 :: [(Int, Int)] -> Int
part1 = sum . map (abs . uncurry (-)) . uncurry zip . bimap sort sort . unzip

part2 :: [(Int, Int)] -> Int
part2 ids = sum . map score $ lefts
  where
    (lefts, rights) = unzip ids
    multiset = fmap (\l -> (head l, length l)) . group . sort $ rights
    score n = n * fromMaybe 0 (lookup n multiset)