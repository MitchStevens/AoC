import Advent
import Point
import Rotation
import Facing
import Data.Array (Array)
import qualified Data.Array as A
import Array2D as A2D
import Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as M
import Control.Monad.Memo


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
  deriving (Eq, Ord)


data Move = RotateLeft | RotateRight | MoveForwards
type Cost = Int

main :: IO ()
main = do
  input <- readInput 2024 16
  let maze = readArray2D id testInput
  print (part1 maze)
  pure ()

cost :: Move -> Int
cost = \case
  MoveForwards -> 1
  _ -> 1000

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


ffff :: Array Point Char -> Raindeer -> Memo Raindeer Cost Cost
ffff maze = f
  where
    f :: Raindeer -> Memo Raindeer Cost Cost
    f deer@(Raindeer pos facing) = 
      case safeLookup maze pos of
        Just 'E' -> pure 0
        Just '#' -> pure maxBound
        Nothing -> pure maxBound
        _ -> minimum <$> sequence
          [ (1000 +) <$> memo f (invertRunMove RotateLeft   deer)
          , (1000 +) <$> memo f (invertRunMove RotateRight  deer)
          , (1 +)    <$> memo f (invertRunMove MoveForwards deer)
          ]

part1 :: Array Point Char -> Int
part1 maze = 
  let Just startPos = A2D.findIndex maze 'S'
  in startEvalMemo (ffff maze (Raindeer startPos R))