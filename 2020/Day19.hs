{-# LANGUAGE LambdaCase, DeriveFunctor #-}
module Day19 where

import Data.Map (Map, (!))
import qualified Data.Map as M
import Control.Monad.Free
import Text.Parsec
import Data.Char
import Data.Text (Text)
import qualified Data.Text as T

type Parser a = Parsec Text () a
data RuleF a = OneOf a a | AndThen a a | ParseChar Char | Reference Int
  deriving (Functor)
type Rule = Free RuleF (Parser ())


eval :: Map Int Rule -> Int -> Parser ()
eval ruleMap ruleNum = iter f (ruleMap ! ruleNum)
  where
    f :: RuleF (Parser ()) -> Parser ()
    f = \case
      OneOf a b -> choice [a, b]
      AndThen a b -> a *> b
      ParseChar c -> char c *> pure ()
      Reference n -> eval ruleMap n

intP :: Parser Int
intP = read <$> many1 digit

ruleP :: Parser (Int, Rule)
ruleP = (,) <$> (intP <* (string ": ")) <*> (subRuleP <* eof)
  where
    subRuleP :: Parser Rule
    subRuleP = (parserTrace "hello" *>) $ wrap <$> choice
      [ OneOf <$> (subRuleP <* string " | ") <*> subRuleP
      , AndThen <$> subRuleP <*> (string " " *> subRuleP)
      , Reference <$> intP
      , ParseChar <$> (char '\"' *> (satisfy (`elem` "ab")) <* char '\"')
      ]
    oneOfP = do
      a <- subRuleP
      string " | "
      b <- subRuleP
      pure $ OneOf a b

    andThen = do


{-
        [Parser (RuleF Rule)]
        choice :: [Parser (RuleF Rule)] -> Parser (RuleF Rule)
        wrap :: f (m a) -> m a
        wrap :: RuleF Rule -> Rule
-}

main :: IO ()
main = do
  file <- T.pack <$> readFile "Day19Example.txt"
  let (rulesStr, recieved) = tail <$> span (T.pack "" /=) (T.lines file)
  print rulesStr
  print recieved
  let Right rules = traverse (parse ruleP "") rulesStr
  print (length rules)
  let ruleMap = M.fromList rules
  pure ()
