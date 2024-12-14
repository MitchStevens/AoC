module Day11 where

import Control.Comonad

newtype Zipper a = Zipper [a] a [a]
newtype Plane a = Plane (getPlane :: Zipper (Zipper a)) deriving Functor

instance Comonad Plane where
  extract = extract . extract . getPlane
  duplicate (Plane plane) = Plane $ (fmap.fmap) Plane $ duplicate (duplicate <$> plane)
    where
      moveLeft (Zipper (l:ls) v rs) = Zipper ls l (v:rs)
      moveRight (Zipper ls v (r:rs)) = Zipper (v:vs) r rs


data Seat = Empty | Occupied

nextState :: Plane Seat -> Seat
nextState = Plane (ls, vs, rs) =


main :: IO ()
main = do
  input <- lines <$> readFile "Day11Input.txt"
