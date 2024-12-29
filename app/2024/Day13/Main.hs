import Advent

import Data.Matrix
import Parsers
import Data.Vector as V
import Data.Ratio

testInput = unlines
  [ "Button A: X+94, Y+34"
  , "Button B: X+22, Y+67"
  , "Prize: X=8400, Y=5400"
  , ""
  , "Button A: X+26, Y+66"
  , "Button B: X+67, Y+21"
  , "Prize: X=12748, Y=12176"
  , ""
  , "Button A: X+17, Y+86"
  , "Button B: X+84, Y+37"
  , "Prize: X=7870, Y=6450"
  , ""
  , "Button A: X+69, Y+23"
  , "Button B: X+27, Y+71"
  , "Prize: X=18641, Y=10279"
  ]

data Machine = Machine
  { buttonA :: Vector Int
  , buttonB :: Vector Int
  , prize   :: Vector Int
  }

machineP :: Parser Machine
machineP = do
  buttonA <- buttonP 'A' <* newline
  buttonA <- buttonP 'B' <* newline
  prize <- do
    x <- toRational <$> chunk "Prize: X=" *> intP
    y <- chunk ", Y=" *> intP
    pure (V.fromList [x, y])
  newline
  pure (Machine buttonA buttonB prize)

  where
    buttonP c = do
      x <- chunk ("Button " <> [c] <> ": X+") *> intP
      y <- chunk ", Y+" *> intP
      pure (V.fromList [x, y])

pivotPositions :: Num a => Matrix a -> [(Int, Int)]
pivotPositions matrix = mapMaybe (\i -> fmap (\j -> (x, j)) . find (/= 0) . (`getRow` matrix) $ i) [0..(numRows matrix - 1)]
  row 


cheapestWinMethod :: Machine -> Maybe (Vector Int)
cheapestWinMethod (Machine a b p) = case rref augmented of
  Left str -> Nothing
  Right solved
    |




  where
    augmented = colVector a <|> colVector b <|> colVector p

    rationalToInt :: Ratio Int -> Maybe Int
    rationalToInt rational = 
      guard (denominator rational == 1) $> numerator rational
    
    aa :: Matrix (Ratio Int) -> Maybe (Vector Int)
    aa rrefMatrix
      | all ()     $ (`getCol` rrefMatrix) <$> [0, 1]




main :: IO ()
main = do
  input <- readInput 2024 13
  pure ()

