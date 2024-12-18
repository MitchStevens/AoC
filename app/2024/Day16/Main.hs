import Advent
import Point
import Rotation
import Facing
import Data.Array (Array)
import qualified Data.Array as A
import Array2D as A2D
import Data.Hashable
import GHC.Generics
import Graph


testInput =
  [ "###############"
  , "#.......#....E#"
  , "#.#.###.#.###.#"
  , "#.....#.#...#.#"
  , "#.###.#####.#.#"
  , "#.#.#.......#.#"
  , "#.#.#####.###.#"
  , "#...........#.#"
  , "###.#.#####.#.#"
  , "#...#.....#.#.#"
  , "#.#.#.###.#.#.#"
  , "#.....#...#.#.#"
  , "#.###.#.#.#.#.#"
  , "#S..#.....#...#"
  , "###############"
  ]

data Raindeer = Raindeer { pos :: Point, facing :: Facing }
  deriving (Eq, Ord, Show, Generic)

instance Hashable Raindeer where
  hash (Raindeer pos facing) = hash facing + 4 * hash pos


data Move = RotateLeft | RotateRight | MoveForwards
type Cost = Int

main :: IO ()
main = do
  input <- lines <$> readInput 2024 16
  let maze = readArray2D id input
  print (part1 maze)
  pure ()

runMove :: Move -> Raindeer -> Raindeer
runMove move (Raindeer pos facing) = case move of
  RotateLeft   -> Raindeer pos (turn (rotation 3) facing)
  RotateRight  -> Raindeer pos (turn (rotation 1) facing)
  MoveForwards -> Raindeer (toDirection facing pos) facing

invertRunMove :: Move -> Raindeer -> Raindeer
invertRunMove move deer@(Raindeer pos facing) = case move of
  RotateLeft   -> runMove RotateRight deer
  RotateRight  -> runMove RotateLeft deer
  MoveForwards -> Raindeer (toDirection (turn 2 facing) pos) facing

nextNode :: Array Point Char -> Raindeer -> [(Raindeer, Int)]
nextNode maze deer@(Raindeer pos facing) = case safeLookup maze pos of
  Just 'E' -> []
  Just '#' -> []
  Nothing  -> []
  _ ->  
    [ (runMove RotateLeft   deer, 1000)
    , (runMove RotateRight  deer, 1000)
    , (runMove MoveForwards deer, 1)
    ]




part1 :: Array Point Char -> Int
part1 maze = fromMaybe maxBound $ do
  pure 1
--  start <- A2D.findIndex maze 'S'
--  goal <- A2D.findIndex maze 'E'
--  fst <$> astarSearch (Raindeer start R) (\deer -> pos deer == goal) (nextNode maze)  (const maxBound)

--part2 :: Array Point Char -> Maybe Int
--part2 maze