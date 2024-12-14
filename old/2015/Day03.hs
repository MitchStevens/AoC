import Data.Monoid
import Data.Foldable
import Control.Monad
import Control.Monad.Trans.State.Strict
import Data.Map.Strict (Map)
import Data.Map.Strict as M
import Text.Parsec hiding (State)

data Direction = N | S | E | W
type Position = (Int, Int)

main :: IO ()
main = do
  file <- readFile "day03.txt"
  let Right directions = parse (many parseDir) "" file
  print $ M.size $ M.filter (>0) $ execState (foldM move (0, 0) directions) M.empty
  let (santa, robot) = splitInstructions directions
  let santaHouses = execState (foldM move (0, 0) santa) M.empty
  let robotHouses = execState (foldM move (0, 0) robot) M.empty
  print $ M.size $ M.filter (>0) $ unionWith (+) robotHouses santaHouses


splitInstructions :: [Direction] -> ([Direction], [Direction])
splitInstructions []       = ([], [])
splitInstructions (x:y:zs) = ([x], [y]) <> (splitInstructions zs)

type Parser a = Parsec String () a

parseDir :: Parser Direction
parseDir = choice
  [ N <$ char '^'
  , S <$ char 'v'
  , E <$ char '<'
  , W <$ char '>'
  ]

move :: Position -> Direction -> State (Map Position Int) Position
move (x, y) dir = newLocation <$ modify (dropGift (x, y))
  where
    newLocation = case dir of
      N -> (x, y-1)
      S -> (x, y+1)
      E -> (x-1, y)
      W -> (x+1, y)

dropGift :: Position -> Map Position Int -> Map Position Int
dropGift pos = M.insertWith (+) pos 1