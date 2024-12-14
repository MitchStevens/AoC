module ReactorReboot where

import Range

type Cuboid = (Range Int, Range Int, Range Int)

newtype Region = Region [Cuboid]

data Instruction
  = TurnOn Cuboid
  | TurnOff Cuboid

type Program = [(Int, Instruction)]

instance Num Region where
  Region r1 + Region r2 = Region (r1 <> r2)
  Region r1 - Region r2 = Region (foldl f_ r1 r2)
    where
      f_ = undefined

subtractCuboid :: Cuboid -> Cuboid -> Region
subtractCuboid = undefined

intersects :: Cuboid -> Cuboid -> Bool
intersects (x1, y1, z1) (x2, y2, z2) =
  x1 `overlapping` x2 && y1 `overlapping` y2 && z1 `overlapping` z2
  where
    overlapping (Range a b) (Range c d) = c < b || a < d

day22 :: IO ()
day22 = do
  print ()
