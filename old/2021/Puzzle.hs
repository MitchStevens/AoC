module Puzzle where

import Data.Foldable
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import qualified Data.Set as S

--data Puzzle piece = Puzzle {possibleCompletions :: S.Set [piece]}

newtype SortingPuzzle = Sorted (NE.NonEmpty Int)
  deriving (Eq, Ord)

instance Show SortingPuzzle where
  show (Sorted m) = foldMap show m

class Piece p where
  rotation :: p -> [p]
  rotation = pure
  isMatch :: p -> p -> Maybe p

matchPieces :: (Monad m, Piece p) => m p -> m p -> m [p]
matchPieces xs ys = do
  x <- xs
  y <- ys
  pure $ maybe [x, y] pure (isMatch x y)

instance Piece SortingPuzzle where
  isMatch (Sorted a) (Sorted b)
    | NE.last a + 1 == NE.head b = Just (Sorted (a <> b))
    | NE.last b + 1 == NE.head a = Just (Sorted (b <> a))
    | otherwise = Nothing

--asPuzzle :: [p] -> Puzzle p
--asPuzzle = Puzzle . S.singleton

-- choose a piece
-- find piece with highest bestMatchScore, merge pieces together
-- iterate as needed

day19 :: IO ()
day19 = do
  let test = Sorted . pure <$> [3, 5, 2, 4, 1]
  -- print $ foldl matchPieces [head test] [tail test]
  print "hello"
