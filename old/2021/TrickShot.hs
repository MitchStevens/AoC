module TrickShot where

import Control.Applicative (liftA2)
import Data.Foldable
import Data.Ix (inRange)
import Data.Ord (comparing)

minX, maxX, minY, maxY :: Int
--(minX, maxX) = (20, 30)
--(minY, maxY) = (-10, -5)
(minX, maxX) = (88, 125)
(minY, maxY) = (-157, -103)

targetX, targetY :: (Int, Int)
targetX = (minX, maxX)
targetY = (minY, maxY)

triangular :: Int -> Int
triangular n = n * (n + 1) `div` 2

trajectoryX :: Int -> [Int]
trajectoryX dx = scanl (+) 0 ([dx, dx - signum dx .. 0] <> repeat 0)

trajectoryY :: Int -> [Int]
trajectoryY dy = scanl (+) 0 [dy, (dy - 1) ..]

trajectory :: Int -> Int -> [(Int, Int)]
trajectory dx dy = zip (trajectoryX dx) (trajectoryY dy)

allXWithinTarget :: [Int]
allXWithinTarget = filter withinTarget rangeX
  where
    rangeX = [min 0 minX .. max 0 maxX]
    withinTarget dx = any (inRange targetX) . take (abs dx) $ trajectoryX dx

allYWithinTarget :: [Int]
allYWithinTarget = filter withinTarget rangeY
  where
    rangeY = [minY .. 2 * abs minY + 1]
    withinTarget dy = any (inRange targetY) . takeWhile (>= minY) $ trajectoryY dy

withinTarget :: Int -> Int -> Bool
withinTarget dx dy = any (\(x, y) -> inRange targetX x && inRange targetY y) . takeWhile (\(x, y) -> y >= minY) $ trajectory dx dy

--target area: x=88..125, y=-157..-103
day17 :: IO ()
day17 = do
  let hitTarget = filter (uncurry withinTarget) $ liftA2 (,) allXWithinTarget allYWithinTarget
  print . triangular . maximum . map snd $ hitTarget
  print . length $ hitTarget

--print $ withinTarget 6 3
--print $ takeWhile (\(x, y) -> y >= minY) $ trajectory 6 3
