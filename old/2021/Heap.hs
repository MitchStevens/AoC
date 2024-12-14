module Heap where

import Data.Heap

data ShortestPolicy

data LongestPolicy

--instance Foldable f => HeapItem ShortestPolicy (f a) where
--  type Prio ShortestPolicy (f a) = Int
--  type Val ShortestPolicy (f a) = f a
--  split fa = (length fa, fa)
--  merge (_, fa) = fa
--
--instance Foldable f => HeapItem LongestPolicy (f a) where
--  type Prio ShortestPolicy (f a) = Int
--  type Val ShortestPolicy (f a) = f a
--  split fa = (length fa, fa)
--  merge (_, fa) = fa

type ShortestHeap a = Heap ShortestPolicy a

type LongestHeap a = Heap LongestPolicy a
