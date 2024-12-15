module Parsers (
  module Text.Megaparsec,
  module Text.Megaparsec.Char,
  type Parser,
  intP, pointP
) where

import Data.Char
import Data.Functor
import Text.Megaparsec
import Text.Megaparsec.Char
import Control.Monad.Combinators
import Data.Void
import Point

type Parser = Parsec Void String

intP :: Parser Int
intP = do
  sign <- char '-' $> (-1) <|> char '+' $> 1 <|> pure 1
  (* sign) . read <$> some digitChar

pointP :: Parser Point
pointP = (,) <$> (intP <* char ',') <*> intP
