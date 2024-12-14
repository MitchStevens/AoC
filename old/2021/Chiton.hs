{-# LANGUAGE FlexibleContexts #-}


module Chiton where

import Advent
import Data.Bifunctor
import Control.Monad.Memo
import Data.Array (Array, (!))
import qualified Data.Array as A
import Point
import Array2D
import Data.Foldable (maximumBy, traverse_)
import Data.Function
import Data.Ix
import Data.Char (digitToInt)

uncoverLargerMap :: Array Point Int -> Array Point Int
uncoverLargerMap m =
  let (width, height) = bimap succ succ . snd $ A.bounds m
      mod9 x = let x' = x `mod` 9 in if x' == 0 then 9 else x'
  in A.array ((0, 0), ((width*5)-1, (height*5)-1)) $ do
    i <- [0..4]
    j <- [0..4]
    (x, y) <- A.indices m
    pure ((x + i*width, y + j*height), mod9 (i + j + m ! (x, y)))

minimumRiskLevel :: Array Point Int -> Int
minimumRiskLevel m = startEvalMemo (f (snd (A.bounds m)))
  where
    f p = case p of
      (0, 0) -> pure 0
      (i, 0) -> (m ! p +) <$> memo f (i-1, 0)
      (0, j) -> (m ! p +) <$> memo f (0, j-1)
      (i, j) -> (m ! p +) <$> (min <$> memo f (i-1, j) <*> memo f (i, j-1))

day15 :: IO ()
day15 = do
  input <- readInput 2021 15
  let riskLevels = readArray2D digitToInt input
  print $ minimumRiskLevel riskLevels
  print $ minimumRiskLevel (uncoverLargerMap riskLevels)
