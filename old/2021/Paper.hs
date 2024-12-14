module Paper where

import Data.Set (Set)
import qualified Data.Set as S
import Data.Foldable
import Data.List
import Advent

type Dot = (Int, Int)
type Paper = Set Dot
data PaperFold = X Int | Y Int

testPaper :: Paper
testPaper = S.fromList $
  [ (6,10)
  , (0,14)
  , (9,10)
  , (0,3)
  , (10,4)
  , (4,11)
  , (6,0)
  , (6,12)
  , (4,1)
  , (0,13)
  , (10,12)
  , (3,4)
  , (3,0)
  , (8,4)
  , (1,10)
  , (2,14)
  , (8,10)
  , (9,0)
  ]

foldPaper :: PaperFold -> Paper -> Paper
foldPaper paperFold = S.map foldDot
  where
    foldDot :: Dot -> Dot
    foldDot (i, j) = case paperFold of
      X x -> (i - max 0 (i - x) * 2, j)
      Y y -> (i, j - max 0 (j-y) * 2)




printPaper :: Paper -> IO ()
printPaper paper = traverse_ print $ do
  j <- [0..height]
  lines $ do
    i <- [0..width]
    pure $ if S.member (i, j) paper then '#' else '.'
  where
    height = maximum . S.map snd $ paper
    width  = maximum . S.map fst $ paper

day13 :: IO ()
day13 = do
  (points, folds) <- break null <$> readInput 2021 13
  let paper = S.fromList $ map ((read :: String -> Dot) . (\s -> "("<>s<>")")) points
  let folds = [ X 655 , Y 447 , X 327 , Y 223 , X 163 , Y 111 , X 81 , Y 55 , X 40 , Y 27 , Y 13 , Y 6]
  print $ length (foldPaper (head folds) paper)
  printPaper (foldl (flip foldPaper) paper folds)
