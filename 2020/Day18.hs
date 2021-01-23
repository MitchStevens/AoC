{-# LANGUAGE LambdaCase, DeriveFunctor, OverloadedStrings #-}
module Day18 where

import Control.Monad.Free
import Text.Parsec
import Text.Show
import Data.Functor.Classes
--import Text (Text)

type Parser a = Parsec String () a
data OperationF a = Add a a | Mul a a | Bracket a
  deriving (Functor)
type Expr = Free OperationF Int

instance Show1 OperationF where
  liftShowsPrec f _ n op =
    let g = f n
    in case op of
      Add x y -> g x . showString " + " . g y
      Mul x y -> g x . showString " * " . g y
      Bracket x -> showString "(" . g x . showString ")"

evaluateExpr :: Expr -> Int
evaluateExpr = iter $ \case
  Add x y -> x + y
  Mul x y -> x * y
  Bracket x -> x

exprP :: Parser Expr
exprP = choice  -- Parser (OperationF Expr)
  [ wrap <$> (Bracket <$> between (char '(') (char ')') exprP)
  , wrap <$> (Add <$> exprP <*> (string " + " *> exprP))
  , wrap <$> (Mul <$> exprP <*> (string " * " *> exprP))
  , pure <$> intP
  ]
  where
    intP = read <$> many1 digit

main :: IO ()
main = do
  expressions <- lines <$> readFile "Day18Input.txt"
  parseTest exprP "" --(head expressions)
  pure ()
