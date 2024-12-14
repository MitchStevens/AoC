module Dive where

import Advent
import Control.Lens
import Parsers
import Point hiding (Direction)
import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec (lift, readPrec_to_S)

data Direction = Forward Int | Down Int | Up Int

data Ship = Ship {_pos :: Point, _aim :: Int}
  deriving (Show)

makeLenses ''Ship

instance Read Direction where
  readsPrec =
    readPrec_to_S . lift . choice $
      [ Forward <$> (string "forward " *> intP),
        Down <$> (string "down " *> intP),
        Up <$> (string "up " *> intP)
      ]

nextShip :: Direction -> Ship -> Ship
nextShip = \case
  Down n -> pos . _2 +~ n
  Up n -> pos . _2 -~ n
  Forward n -> pos . _1 +~ n

nextShip' :: Direction -> Ship -> Ship
nextShip' = \case
  Down n -> aim +~ n
  Up n -> aim -~ n
  Forward n -> \ship ->
    ship
      & pos . _1 +~ n
      & pos . _2 +~ (ship ^. aim) * n

runSimulation :: (Direction -> Ship -> Ship) -> [Direction] -> Ship
runSimulation next = foldl (flip next) (Ship (0, 0) 0)

day2 :: IO ()
day2 = do
  course <- map (read :: String -> Direction) <$> readInput 2021 2
  --let course = [Forward 5, Down 5, Forward 8, Up 3, Down 8, Forward 2]
  let score (Ship (x, y) _) = x * y
  print . score $ runSimulation nextShip course
  print . score $ runSimulation nextShip' course
