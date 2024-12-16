module Facing where

import Point
import Rotation

data Facing = U | R | D | L
  deriving (Eq, Ord, Enum)

rotationBetween :: Facing -> Facing -> Rotation
rotationBetween a b = rotation (fromEnum b - fromEnum a)

turn :: Rotation -> Facing -> Facing
turn (Rotation n) f = case (fromEnum f + n) `mod` 4 of
  0 -> U
  1 -> R
  2 -> D
  3 -> L
  _ -> undefined

toDirection :: Facing -> Direction
toDirection = \case
  U -> up
  R -> right
  D -> down
  L -> left