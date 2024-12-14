module OperationOrder where

import Data.Functor
import Data.Void
import Text.Megaparsec
import Advent
import Text.Megaparsec.Char
import Data.Foldable
import Data.Char
import Data.Either

type Parser = Parsec Void String

data Expression
    = Addition 
    | Multiplication
    | Literal Int
    | Parens [Expression]

instance Show Expression where
    show = \case
        Addition -> "+"
        Multiplication -> "*"
        Literal n -> show n
        Parens exps -> "(" <> unwords (fmap show exps) <> ")"

evalExpression :: [Expression] -> Int
evalExpression [Literal a] = a
evalExpression (e1:Addition:e2:es) =
    let e1' = evalExpression [e1]
        e2' = evalExpression [e2]
    in evalExpression $ Literal (e1' + e2') : es
evalExpression (e1:Multiplication:e2:es) =
    let e1' = evalExpression [e1]
        e2' = evalExpression [e2]
    in evalExpression $ Literal (e1' * e2') : es
evalExpression (Parens exp:es) =
    let n = evalExpression exp
    in evalExpression (Literal n:es)

expressionP :: Parser [Expression]
expressionP = (`sepBy` space) . choice . map try $
    [ Literal . read <$> takeWhile1P Nothing isDigit
    , char '+'$> Addition
    , char '*' $> Multiplication
    , Parens <$> between (char '(') (char ')') (try expressionP)
    ]

testInput = 
    [ "1 + 2 * 3 + 4 * 5 + 6"
    , "2 * 3 + (4 * 5)"
    , "5 + (8 * 3 + 9 + 3 * 4 * 3)"
    , "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"
    , "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
    ]

day18 :: IO ()
day18 = do
    --let input = testInput
    input <- readInput 2020 18
    --print $ evalExpression [Literal 1]
    --print $ evalExpression [Literal 1, Addition, Literal 2]
    --print $ evalExpression [Parens [Literal 1]]
    let expressions = rights . map (parse expressionP "") $ input
    print . sum . fmap evalExpression $ expressions
    --print . sequence . fmap evalExpression . map (parse expressionP "") $ input

