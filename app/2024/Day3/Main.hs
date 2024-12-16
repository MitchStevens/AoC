{-# LANGUAGE OverloadedStrings #-}

import Advent
import Data.List
import Data.Bifunctor
import Data.Maybe
import Control.Monad
import Control.Comonad
import Zipper
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators
import Data.Void

type Parser = Parsec Void String

data Instruction
  = Mul Int Int
  | Enable
  | Disable
  deriving (Show)

runInst :: Instruction -> Int
runInst (Mul a b) = a * b
runInst _ = 0

main = do
  input <- readInput 2024 3
  --let input = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"
  --let input = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"
  let parser = catMaybes <$> many (try (Just <$> instructionP) <|> (anySingle $> Nothing))
  let Right instructions = parse parser "" input
  print (show (instructions))
  print (show (part1 instructions))
  print (show (part2 instructions))

intP :: Parser Int
intP = do
  a <- digitChar
  b <- maybeToList <$> optional digitChar
  c <- maybeToList <$> optional digitChar
  pure $ read $ [ a ] <> b <> c

instructionP :: Parser Instruction
instructionP = mulP <|> enableP <|> disableP
  where
    mulP = do
      string "mul("
      a <- intP
      string ","
      b <- intP
      string ")"
      pure (Mul a b)
    
    enableP  = string "do()" $> Enable
    disableP = string "don't()" $> Disable

part1 :: [Instruction] -> Int
part1 = sum . fmap runInst

part2 :: [Instruction] -> Int
part2 = part2Rec True
  where
    part2Rec :: Bool -> [Instruction] -> Int
    part2Rec _ [] = 0
    part2Rec enabled (x:xs) = case x of
      Mul a b -> (if enabled then a * b else 0) + part2Rec enabled xs
      Enable -> part2Rec True xs
      Disable -> part2Rec False xs

--part2 :: [Zipper Int] -> Int
--part2 = length . filter and . fmap (extend isSafeWithDampener)