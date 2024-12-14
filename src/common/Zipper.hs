module Zipper where

import Control.Comonad
import Data.List 

data Zipper a = Zipper [a] a [a]

moveLeft :: Zipper a -> Maybe (Zipper a)
moveLeft (Zipper (l:ls) v rs) = Just (Zipper ls l (v:rs))
moveLeft _ = Nothing

moveRight :: Zipper a -> Maybe (Zipper a)
moveRight (Zipper ls v (r:rs)) = Just (Zipper (v:ls) r rs)
moveRight _ = Nothing

toZipper :: [a] -> Zipper a
toZipper (l:ls) = Zipper [] l ls

instance Functor Zipper where
  fmap f (Zipper ls v rs) = Zipper (fmap f ls) (f v) (fmap f rs) 

instance Foldable Zipper where
  -- foldl f z (Zipper ls v rs) = foldl f (f (foldr (flip f) z ls) v) rs
  foldr f z (Zipper ls v rs) = foldr f (f v (foldl (flip f) z ls)) rs

instance Comonad Zipper where
  extract (Zipper _ v _) = v
  extend f zipper@(Zipper ls v rs) = Zipper (map f allLefts) (f zipper) (map f allRights)
    where
      allLefts  = unfoldr (\z -> let z' = moveLeft  z in (,) <$> z' <*> z') zipper
      allRights = unfoldr (\z -> let z' = moveRight z in (,) <$> z' <*> z') zipper
