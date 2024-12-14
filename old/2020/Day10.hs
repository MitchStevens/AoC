-- |
{-# LANGUAGE TypeApplications #-}
module Main where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map (Map)
import Data.Ix
import Data.List
import Control.Monad

type Adapter = Int
type JoltageChain = [Adapter]

initialJoltage = [0]

countJoltage :: [Adapter] -> Int -> Int
countJoltage adapters goal = go 0
  where
    go :: Int -> Int
    go n = if n == goal then 1 else sum . map (m M.!) . filter (inRange (n+1, n+3)) $ adapters

    m :: Map Int Int
    m = M.fromSet go (S.fromList adapters)



--countJoltage :: JoltageChain -> Adapter ->
--countJoltage [] goal = 0
--countJoltage (adapter:as) goal
--  | goal-adapter <= 3 = countJoltage as goal + countJoltage as adapter
--  | otherwise = 0

test = [16,10,15,5,1,11,7,19,6,12,4]
test2 = [28,33,18,42,31,14,46,20,48,47,24,23,49,45,19,38,39,11,1,32,25,35,8,17,7,9,4,2,34,10,3]


main :: IO ()
main = do
  input <- map (read @Int) . lines <$> readFile "Day10Input.txt"
  let goal = maximum input + 3
  let adapters = sort ([0, goal] <> input)
  let oneJolt   = length . filter (==1) $ zipWith (-) (tail adapters) adapters
  let threeJolt = length . filter (==3) $ zipWith (-) (tail adapters) adapters
  print (oneJolt * threeJolt)
  print $ countJoltage adapters goal
