module Array2D where

import Data.Array (Array, (!))
import qualified Data.Array as A
import Data.Map (Map)
import Data.Function (fix)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tuple
import Data.Maybe
import Point
import Data.List as L
import Data.Ix
import Data.Functor.WithIndex
import Data.Foldable.WithIndex
import Algebra.Graph
import Control.Monad

showArray2D :: Array Point Char -> String
showArray2D arr = unlines (map line [0..y])
    where
        line n = look (0,n) right arr
        (_, (_, y)) = A.bounds arr

readArray2D :: (a -> b) -> [[a]] -> Array Point b
readArray2D f strs = A.listArray ((0, 0), top) (L.transpose strs >>= map f)
    where top = (length strs -1, length (head strs) -1)

extendArray2D :: (Array Point a -> Point -> b) -> Array Point a -> Array Point b
extendArray2D f arr = A.listArray (A.bounds arr) (f arr <$> A.indices arr)

groupArray2D :: Ord a => Array Point a -> Map a (Set Point)
groupArray2D = foldl (M.unionWith mappend) M.empty . imap (\i a -> M.singleton a (S.singleton i))

safeLookup :: Ix i => Array i a -> i -> Maybe a
safeLookup arr i = if inRange (A.bounds arr) i then Just (arr ! i) else Nothing

lookupMany :: Array Point a -> [Direction] -> Point -> [a]
lookupMany arr dirs point = mapMaybe (\dir -> safeLookup arr (dir point)) dirs

look :: Point -> Direction -> Array Point a -> [a]
look point dir arr = unfoldr f point
    where f p = safeLookup arr p >>= \a -> Just (a, dir p)

tabulate :: Ix i => (i,i) -> (i -> a) -> Array i a
tabulate rng f = A.listArray rng (map f $ range rng)

recurse :: Ix i => Array i a -> (Array i b -> i -> b) -> Array i b
recurse arr f = fix (tabulate (A.bounds arr) . f)

findIndex :: (Eq e, Ix i) => Array i e -> e -> Maybe i
findIndex arr target = ifoldl (\i maybeInd e -> if e == target then Just i else maybeInd) Nothing arr

--toGraph :: Array Point a -> Graph (Point, a)
--toGraph array2D = fmap (\p -> (p, array2D ! p)) skeleton
--    where
--        skeleton :: Graph Point
--        skeleton = edges $ do
--            p <- range (A.bounds array2D)
--            a <- cardinal p
--            guard (inRange (A.bounds array2D) a)
--            pure (p, a)

{-
vertexMap :: a -> g (VertexMap g a)
vertexMap :: a -> Array Point [Point] -> (VertexMap g a)
vertexMap :: a -> Array Point [Point] -> (PropertyMap (Array Point [Point] -> ??) Point a)

-}