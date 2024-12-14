module Array2D where

import Data.Array (Array, (!))
import qualified Data.Array as A
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Point
import Data.List (unfoldr, transpose)
import Data.Ix
import Data.Functor.WithIndex

showArray2D :: Show a => Array Point a -> String
showArray2D arr = unlines (unfoldr line (A.elems arr))
    where
        (_, (x1, _)) = A.bounds arr
        line [] = Nothing
        line xs = Just (map (head . show) (take (x1+1) xs), drop (x1+1) xs)

readArray2D :: (a -> b) -> [[a]] -> Array Point b
readArray2D f strs = A.listArray ((0, 0), top) (transpose strs >>= map f)
    where top = (length strs -1, length (head strs) -1)

extendArray2D :: (Array Point a -> Point -> b) -> Array Point a -> Array Point b
extendArray2D f arr = A.listArray (A.bounds arr) (f arr <$> A.indices arr)

groupArray2D :: Ord a => Array Point a -> Map a (Set Point)
groupArray2D = foldl (M.unionWith mappend) M.empty . imap (\i a -> M.singleton a (S.singleton i))

safeLookup :: Ix i => Array i a -> i -> Maybe a
safeLookup arr i = if inRange (A.bounds arr) i then Just (arr ! i) else Nothing

look :: Point -> Direction -> Array Point a -> [a]
look point dir arr = unfoldr f point
    where f p = safeLookup arr p >>= \a -> Just (a, dir p)

tabulate :: Ix i => (i,i) -> (i -> a) -> Array i a
tabulate rng f = A.listArray rng (map f $ range rng)

recurse :: Ix i => Array i a -> (Array i b -> i -> b) -> Array i b
recurse arr f = 
    let m = tabulate (A.bounds arr) (f m)
    in m
