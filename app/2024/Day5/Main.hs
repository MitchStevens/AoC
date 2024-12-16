{-# LANGUAGE OverloadedStrings #-}

import Advent
import Algebra.Graph.AdjacencyMap
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as M
import qualified Algebra.Graph.Acyclic.AdjacencyMap as Acyclic
import Parsers

testInput =
  [ "47|53"
  , "97|13"
  , "97|61"
  , "97|47"
  , "75|29"
  , "61|13"
  , "75|53"
  , "29|13"
  , "97|29"
  , "53|29"
  , "61|53"
  , "97|53"
  , "61|29"
  , "47|13"
  , "75|47"
  , "97|75"
  , "47|61"
  , "75|61"
  , "47|29"
  , "75|13"
  , "53|13"
  , ""
  , "75,47,61,53,29"
  , "97,61,53,29,13"
  , "75,29,13"
  , "75,97,47,61,53"
  , "61,13,29"
  , "97,13,75,29,47"
  ]

type OrderingRule = (Int, Int)
type Updates = [Int]

main = do
  input <- lines <$> readInput 2024 5
  let (orderingInput, updatesInput) = break null input
  let Right orderingRules = runParser (orderingRuleP `sepEndBy` newline) "" (unlines orderingInput)
  let Right updates = runParser (updatesP `sepEndBy` newline) "" (unlines (tail updatesInput))

  let orderingGraph = foldMap (\(a, b) -> edge a b) orderingRules
  print (part1 orderingGraph updates)
  print (part2 orderingGraph updates)


orderingRuleP :: Parser OrderingRule
orderingRuleP = (,) <$> (intP <* char '|') <*> intP

updatesP :: Parser Updates
updatesP = intP `sepEndBy1` char ','

fixUpdates :: AdjacencyMap Int -> Updates -> Updates
fixUpdates rules updates = maybe undefined Acyclic.topSort . Acyclic.toAcyclic . induce (`elem` updates) $ rules

rightOrder :: AdjacencyMap Int -> Updates -> Bool
rightOrder rules updates = case updates of
  x:y:zs -> not (hasEdge y x rules) && rightOrder rules (y:zs)
  _ -> True

middlePage :: Updates -> Int 
middlePage updates = updates !! (length updates `div` 2)

part1 :: AdjacencyMap Int -> [Updates] -> Int
part1 rules = sum . map middlePage . filter (rightOrder rules) 

part2 :: AdjacencyMap Int -> [Updates] -> Int
part2 rules = sum . map middlePage . map (fixUpdates rules) . filter (not . rightOrder rules) 