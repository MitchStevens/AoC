import Advent
import Parsers
import qualified Data.List.NonEmpty as NE
import Point as Point
import Data.Foldable
import Array2D
import Data.Array as A
import Data.Ord
import qualified Data.Set as S
import Data.Maybe
import Data.Bifunctor

testInput = unlines
  [ "p=0,4 v=3,-3"
  , "p=6,3 v=-1,-3"
  , "p=10,3 v=-1,2"
  , "p=2,0 v=2,-1"
  , "p=0,0 v=1,3"
  , "p=3,0 v=-2,-2"
  , "p=7,6 v=-1,-3"
  , "p=3,0 v=-1,-2"
  , "p=9,3 v=2,3"
  , "p=7,3 v=-1,2"
  , "p=2,4 v=2,-3"
  , "p=9,5 v=-3,-3"
  ]

data Robot = Robot { pos :: Point , vel :: Point }
  deriving (Eq, Show)

--(maxX, maxY) = (11, 7)
(maxX, maxY) = (101, 103)

main :: IO ()
main = do
  input <- readInput 2024 14
  let Right robots = runParser (robotP `sepEndBy` newline) "" input
  print (part1 robots)
  print (part2 robots)

robotP :: Parser Robot
robotP = do
  pos <- chunk "p=" *> pointP
  char ' '
  vel <- chunk "v=" *> pointP
  pure (Robot pos vel)

step :: Robot -> Robot
step (Robot pos vel) = (Robot (bimap (`mod` maxX) (`mod` maxY) $ pos + vel) vel)

quadrant :: Robot -> Maybe Int
quadrant (Robot (x, y) _)
  | x < midX && y < midY = Just 1
  | x > midX && y < midY = Just 2
  | x < midX && y > midY = Just 3
  | x > midX && y > midY = Just 4
  | otherwise            = Nothing
  where
    midX = (maxX `div` 2)
    midY = (maxY `div` 2)

printRobots :: [Robot] -> String
printRobots robots = showArray2D array
  where
    array = (A.listArray (origin, (maxX-1, maxY-1)) (repeat '_')) A.// (map (\r -> (pos r, '*')) robots)

imageLikeScore :: [Robot] -> Int
imageLikeScore = length . group . sort . map pos
  
--  sum . map (length . lookupMany array adjacentDirections) $ points
--  where
--    points = map pos robots
--    array = (A.listArray (origin, (maxX-1, maxY-1)) (repeat False)) A.// (map (,True) points)

part1 :: [Robot] -> Int
part1 = product . map length . NE.groupWith quadrant . sortOn quadrant . filter (isJust . quadrant) . map ((!! 100) . iterate step)

part2 :: [Robot] -> Maybe Int
part2 robots = elemIndex xmasTree robotStates
  where
    robotStates = take (maxX * maxY) . iterate (map step) $ robots
    xmasTree = maximumBy (comparing imageLikeScore) robotStates
