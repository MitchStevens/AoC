module Advent (
    getInput,
    readInput,
    distinctPairs,
    module Control.Monad,
    module Data.List,
) where

import Data.Map (Map)
import Data.Set (Set)
import Data.Array (Array)
import qualified Data.Map as M
import Data.Set (Set)
import Data.Bifunctor (Bifunctor(first), bimap)
import Data.Foldable (maximumBy, Foldable (fold))
import Data.Function (on)
import Data.List
import Data.Semigroup
import Control.Monad

--type Solution s = Solution
--    { year :: Int
--    , day :: Int
--    , part1 :: Maybe (s -> String)
--    , part2 :: Maybe (s -> String)
--    }


getInput :: Int -> IO [String]
getInput = undefined

readInput :: Int -> Int -> IO String
readInput year day = readFile
    ("inputs/" <> show year <> "/day" <> show day <> ".txt")

steadyState :: Eq a => (a -> a) -> a -> (Int, a)
steadyState f a = if f a == a then (0, a) else first succ (steadyState f (f a))

distinctPairs :: Ord a => [a] -> [(a, a)]
distinctPairs values = do
    a <- values
    b <- values
    guard (a < b)
    pure (a, b)