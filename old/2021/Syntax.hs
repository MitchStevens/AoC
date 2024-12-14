module Syntax where

import Control.Monad
import Witherable
import Data.List
import Advent

newtype Bracket = Bracket Char
  deriving Show
data SyntaxError = Corrupted { illegalChar :: Bracket } | Incomplete { stack :: [Bracket] }

isCorrupt :: SyntaxError -> Bool
isCorrupt (Corrupted _) = True
isCorrupt _ = False

matches :: Bracket -> Bracket -> Bool
matches (Bracket '[') (Bracket ']') = True
matches (Bracket '(') (Bracket ')') = True
matches (Bracket '{') (Bracket '}') = True
matches (Bracket '<') (Bracket '>') = True
matches _ _ = False

isOpen :: Bracket -> Bool
isOpen (Bracket b) = b `elem` "[({<"

checkBracket :: Bracket -> [Bracket] -> Either (Bracket, [Bracket]) [Bracket]
checkBracket bracket stack
  | isOpen bracket = Right (bracket : stack)
  | null stack = Left (bracket, [])
  | otherwise = if head stack `matches` bracket then Right (tail stack) else Left (bracket, stack)

checkSyntax :: [Bracket] -> Maybe SyntaxError
checkSyntax line = case foldM (flip checkBracket) [] line of
  Left (b, _) -> Just (Corrupted b)
  Right [] -> Nothing
  Right xs -> Just (Incomplete xs)

corruptedScore :: SyntaxError -> Int
corruptedScore = \case
 Corrupted (Bracket ')') -> 3
 Corrupted (Bracket ']') -> 57
 Corrupted (Bracket '}') -> 1197
 Corrupted (Bracket '>') -> 25137
 _ -> 0

incompleteScore :: SyntaxError -> Int
incompleteScore (Corrupted _) = 0
incompleteScore (Incomplete stack) = sum $
  zipWith (*) [5^n | n <- [0..]] (reverse $ map f stack)
  where
    f = \case
      Bracket '(' -> 1
      Bracket '[' -> 2
      Bracket '{' -> 3
      Bracket '<' -> 4
      _ -> 0

median :: [a] -> a
median [] = error "no median!"
median [x] = x
median (x:xs) = median (init xs)

day10 :: IO ()
day10 = do
  input <- readInput 2021 10
  let codeLines = (fmap.fmap) Bracket input
  let (corrupt, incomplete) = partition isCorrupt (mapMaybe checkSyntax codeLines)
  print . sum . fmap corruptedScore $ corrupt
  print . median . sort . fmap incompleteScore $ incomplete
