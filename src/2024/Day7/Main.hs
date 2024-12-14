{-# LANGUAGE OverloadedStrings #-}

import Advent
import Data.Array (Array)
import Data.List
import Control.Monad
import Array2D
import Point
import Data.Maybe
import Parsers

testInput = unlines $
  [ "190: 10 19"
  , "3267: 81 40 27"
  , "83: 17 5"
  , "156: 15 6"
  , "7290: 6 8 6 15"
  , "161011: 16 10 13"
  , "192: 17 8 14"
  , "21037: 9 7 18 13"
  , "292: 11 6 16 20"
  ]

data Equation = Equation { total :: Int, values :: [Int]}
  deriving (Eq, Show)

operators :: [Int -> Int -> Int]
operators = [(+), (*)]

data Tree 
  = Add Tree Tree
  | Mul Tree Tree
  | Conc Tree Tree
  | Value Int
  deriving (Show)

evalTree :: Tree -> Int
evalTree tree = case tree of
  Add a b -> evalTree a + evalTree b
  Mul a b -> evalTree a * evalTree b
  Conc a b -> read $ show (evalTree a) <> show (evalTree b)
  Value n -> n

allAddMulTrees :: [Int] -> [Tree]
allAddMulTrees [] = []
allAddMulTrees (x:xs) = 
  foldl (\trees n -> [Add (Value n), Mul (Value n)] <*> trees) [Value x] xs

allTrees :: [Int] -> [Tree]
allTrees [] = []
allTrees (x:xs) = 
  foldl (\trees n -> [Add (Value n), Mul (Value n), flip Conc (Value n)] <*> trees) [Value x] xs

main = do
  input <- readInput 2024 7
  --parseTest (sepEndBy equationP newline) testInput

  let Right equations = runParser (sepEndBy equationP newline) "" input
  print (show (part1 equations))
  print (show (part2 equations))

equationP :: Parser Equation
equationP = do
  total <- intP
  chunk ": "
  values <- sepBy intP (char ' ')
  pure (Equation total values)

isPart1Solvable :: Equation -> Bool
isPart1Solvable (Equation total values) = any (total ==) (evalTree <$> allAddMulTrees values)

isPart2Solvable :: Equation -> Bool
isPart2Solvable (Equation total values) = any (total ==) (evalTree <$> allTrees values)

part1 :: [Equation] -> Int
part1 = sum . map total . filter isPart1Solvable
  
part2 :: [Equation] -> Int
part2 = sum . map total . filter isPart2Solvable