module SupplyStacks where

import Advent
import qualified Data.Map as M
import Control.Monad.State
import Data.Map (Map)
import Data.Foldable (traverse_)
import Control.Lens
import Data.Maybe (fromJust)

type Stacks = Map Int [Char]

data Rearrange = Rearrange Int Int Int
    deriving Show

pop :: (MonadState Stacks m) => Int -> m Char
pop n = do
    c <- fromJust <$> preuses (ix n) head
    ix n %= tail
    pure c

rearrange :: (MonadState Stacks m) => Rearrange -> m ()
rearrange (Rearrange 0 _ _) = pure ()
rearrange (Rearrange n from to) = do
    c <- pop from
    ix to %= (c:)
    rearrange (Rearrange (n-1) from to)

crateMover :: (MonadState Stacks m) => Rearrange -> m ()
crateMover (Rearrange n from to) = do
    (heads, tails) <- fromJust <$> preuses (ix from) (splitAt n)
    ix from .= tails
    ix to %= (heads <> )

parseRearrangement :: String -> Rearrange
parseRearrangement str = Rearrange (read count) (read from) (read to)
    where [_, count, _, from, _, to] = words str

topCrates :: Stacks -> String
topCrates = map head . M.elems

testStacks :: Stacks
testStacks = M.fromList
    [ (1, "NZ")
    , (2, "DCM")
    , (3, "P")
    ]

testProcedure =
    [ Rearrange 1 2 1
    , Rearrange 3 1 3
    , Rearrange 2 2 1
    , Rearrange 1 1 2
    ]

stacks :: Stacks
stacks = M.fromList
    [ (1, "VQWMBNZC")
    , (2, "BCWRZH")
    , (3, "JRQF")
    , (4, "TMNFHWSZ")
    , (5, "PQNLWFG")
    , (6, "WPL")
    , (7, "JQCGRDBV")
    , (8, "WBNQZ")
    , (9, "JTGCFLH")
    ]

day5 :: IO ()
day5 = do
    input <- readInput 2022 5
    let procedure = map parseRearrangement input
    print . topCrates $ traverse_ rearrange procedure `execState` stacks
    print . topCrates $ traverse_ crateMover procedure `execState` stacks
