module Rotation where

newtype Rotation = Rotation Int

instance Num Rotation where
  Rotation a + Rotation b = rotation (a+b)
  Rotation a * Rotation b = rotation (a*b)
  Rotation a - Rotation b = rotation (a-b)
  negate (Rotation a) = rotation (-a)
  fromInteger n = rotation (fromInteger n)

rotation :: Int -> Rotation
rotation n = Rotation (n `mod` 4)