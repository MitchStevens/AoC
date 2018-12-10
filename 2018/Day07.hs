{-# LANGUAGE TemplateHaskell #-}
import Text.Parsec (Parsec, string, letter, sepBy, newline, parse)
import Data.Char (ord)
import Data.List
import Data.Monoid
import Data.Array
import Data.Traversable (for)
import Control.Monad.State.Strict

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Algebra.Graph.AdjacencyMap
import Control.Monad.Trans.State.Strict
import Control.Lens

data Elf = Working Char Int | Idle
makePrisms ''Elf

data ElfState = ElfState
  { _time :: Int
  , _jobs :: Set Char
  , _graph :: AdjacencyMap Char
  , _elves :: [Elf] }
makeLenses ''ElfState

main :: IO ()
main = do
  file <- readFile "Day07.txt"
  let Right es = parse (parseEdge `sepBy` newline) "" file
  let graph = edges es
  print $ orderSteps graph
  print $ adjacencyMap graph

orderSteps :: AdjacencyMap Char -> [Char]
orderSteps g
  | isEmpty g = []
  | otherwise = o : orderSteps (removeVertex o g)
    where o = minimum . M.keys . M.filter S.null $ adjacencyMap g

parseEdge :: Parsec String () (Char, Char)
parseEdge = do
  string "Step "
  from <- letter
  string " must be finished before step "
  to <- letter
  string " can begin."
  pure (to, from)

iter :: State ElfState ()
iter = do
  employElves
  time += 1
  b <- isFinished
  unless b iter

isFinished :: State ElfState Bool
isFinished = use (graph . to isEmpty)

employElves :: State ElfState ()
employElves = do
  modify ()
  modifying (elves . traverse) =<< x
  where x = sequence workElf :: State ElfState (Elf -> Elf)

workElf :: Elf -> State ElfState Elf
workElf elf = case elf of
  Idle -> maybe Idle newJob <$> nextJob
  Working c r ->
    if r == 0
      then finishJob c *> workElf Idle
      else pure $ Working c (r-1)

newJob :: Char -> Elf
newJob c = Working c (61 + ord c - ord 'A')

nextJob :: State ElfState (Maybe Char)
nextJob = do
  maybeJob <- preuse (jobs.folded)
  for maybeJob $ \job -> do
    jobs %= S.delete job
    pure job

finishJob :: Char -> State ElfState ()
finishJob job = do
  newJobs <- use $ graph . to (postSet job)
  jobs <>= newJobs