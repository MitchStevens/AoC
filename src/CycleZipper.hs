module CycleZipper where

import Control.Comonad
import Data.List.NonEmpty (NonEmpty, (:|))
import qualified Data.List.NonEmpty as NE

data CycleZipper a = CZ
  { bn :: Int
  , bs :: [a]
  , value :: a
  , fn :: Int
  , fs :: [a]
  }

instance Functor CycleZipper where
  fmap f (CZ bn bs value fn fs) = CZ bn (map f bs) (f value) fn (map f fs)

instance Foldable CycleZipper where
  foldr f z (CZ _ bs v _ fs) = foldr f (f v (foldl (flip f) z bs)) fs

instance Comonad CycleZipper where
  extract = value
  duplicate cz@(CZ bn bs v fn fs) = CZ bn bs' cz fn fs'
    where
      bs' = drop 1 . take (bn+1) . iterate moveLeft  $ cz
      fs' = drop 1 . take (fn+1) . iterate moveRight $ cz

fromNonEmptyList :: NonEmpty a -> CycleZipper a
fromNonEmptyList (x :| xs) = CZ 0 [] x (length xs) xs

fromList :: [a] -> Maybe (CycleZipper a)
fromList = fromNonEmptyList . NE.nonEmpty

size :: CycleZipper a -> Int
size (CZ bn _ _ fn _) = 1 + bn + fn

left :: Int -> CycleZipper a -> a
left n = right (-n)

right :: Int -> CycleZipper a -> a
right n cz 
  | n' == 0       = value cz
  | n' <= fn cz   = fs cz !! n'
  | otherwise     = bs cz !! ((bn cz) -1 + n')
  where
    n' = n `mod` size cz

moveLeft :: CycleZipper a -> CycleZipper a
moveLeft (CZ _ [] v fn fs) = case reverse fs of
  b:bs -> CZ (fn-1) bs b 1 [v] 
  []   -> CZ 0      [] v 0 []
moveLeft (CZ bn (b:bs) v fn fs) =
  CZ (bn-1) bs b (fn+1) (fs)

moveRight :: CycleZipper a -> CycleZipper a
moveRight (CZ bn bs v _ []) = case reverse bs of
  f:fs -> CZ 1 [v] f (bn-1) fs
  []   -> CZ 0 []  v 0 []
moveRight (CZ bn bs v fn (f:fs)) =
  CZ (bn+1) (v:bs) f (fn+1) fs
