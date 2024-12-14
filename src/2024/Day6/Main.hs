{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (Left, Right)
import Advent
import Array2D
import Data.Array (Array, (!), (//))
import qualified Data.Array as A
import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Ix
import Data.Maybe
import Data.Foldable.WithIndex
import Point

testInput =
  [ "....#....."
  , ".........#"
  , ".........."
  , "..#......."
  , ".......#.."
  , ".........."
  , ".#..^....."
  , "........#."
  , "#........."
  , "......#..."
  ]

main = do
  input <- lines <$> readInput 2024 6
  let array2D = readArray2D id input
  let obstructions = fmap (\c -> if c == '^' then '.' else c) array2D
  let Just initial = ifoldr (\loc c maybeIndex -> if c == '^' then Just loc else maybeIndex) Nothing array2D
  print (part1 obstructions initial)
  print (part2 obstructions initial)


  --print . fmap location . take 10 $ unfoldr (fmap (\a -> (a, a)) . guardWalk obstructions) (Guard Up initial)


data FacingDirection = Up | Right | Down | Left
  deriving (Eq, Ord, Show)

toDirection :: FacingDirection -> Direction
toDirection fd = case fd of
  Up -> up
  Right -> right
  Down -> down
  Left -> left

turnRight :: FacingDirection -> FacingDirection
turnRight fd = case fd of
  Up -> Right
  Right -> Down
  Down -> Left
  Left -> Up


data Guard = Guard { facing :: FacingDirection, location :: Point }
  deriving (Eq, Ord, Show)

guardWalk :: Array Point Char -> Guard -> Maybe Guard
guardWalk obstructions (Guard facing location) = do
  let next = toDirection facing location
  c <- safeLookup obstructions next

  if c == '#'
    then pure (Guard (turnRight facing) location)
    else pure (Guard facing next)

runGuard :: Array Point Char -> Point -> [Guard]
runGuard obstructions initial =
  unfoldr (fmap (\a -> (a, a)) . guardWalk obstructions) (Guard Up initial)

containsDuplicates :: Ord a => [a] -> Bool
containsDuplicates = containsDuplicatesAcc S.empty
  where
    containsDuplicatesAcc :: Ord a => Set a -> [a] -> Bool
    containsDuplicatesAcc seen [] = False
    containsDuplicatesAcc seen (x:xs) =
      if S.member x seen
        then True 
        else containsDuplicatesAcc (S.insert x seen) xs

part1 :: Array Point Char -> Point -> Int
part1 obstructions initial = 
  length . nub . map location $ runGuard obstructions initial

part2 :: Array Point Char -> Point -> Int
part2 obstructions initial = length . filter (containsDuplicates) . map (\arr -> runGuard arr initial) $ possibleParadoxes
  where
    allLocationsWalked = nub . map location . tail $ runGuard obstructions initial
    possibleParadoxes :: [Array Point Char]
    possibleParadoxes = map (\i -> obstructions // [(i, '#')]) allLocationsWalked