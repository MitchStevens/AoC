module Snailfish where

import Text.Megaparsec.Char
import Data.Char (isDigit)
import Data.Void
import Text.Megaparsec
import Advent (readInput)
import Control.Monad.Free

type Parser = Parsec Void String

data SnailfishF a = Pair a a
  deriving (Eq)

type SnailFish = Free SnailfishF Int

--iterA :: (f (p a) -> p a) -> Free f a -> p a




--instance Show SnailFish where
--  show = iter showPair . fmap show
--    where showPair (Pair a b) = "[" <> show a <> "," <> show b <> "]"
  
--instance Semigroup SnailFish where
--  s1 <> s2 = explode $ wrap (Pair s1 s2)

explode :: SnailFish -> SnailFish
explode = undefined

--[[[[[9,8],1],2],3],4]

--split :: SnailFish -> SnailFish
--split = (=<<) $ \n ->
--  if n < 10
--    then pure n 
--    else liftF $ Pair (n `div` 2) (n - n `div` 2)

snailfishP :: Parser SnailFish
snailfishP = undefined
--snailfishP = choice 
--  [ try (Regular <$> intP)
--  , between (char '[') (char ']') $
--      Pair
--        <$> snailfishP <* char ','
--        <*> snailfishP
--  ]
--  where intP = read <$> takeWhile1P Nothing isDigit

test1 =
  [ "[1,2]"
  , "[[1,2],3]"
  , "[9,[8,7]]"
  , "[[1,9],[8,5]]"
  , "[[[[1,2],[3,4]],[[5,6],[7,8]]],9]"
  , "[[[9,[3,8]],[[0,9],6]],[[[3,7],[4,9]],3]]"
  , "[[[[1,3],[5,3]],[[1,3],[8,7]]],[[[4,9],[6,9]],[[8,2],[7,3]]]]"
  ]

day18 :: IO ()
day18 = do
  --input <- readInput 2021 18
  let input = test1
  let Right snailfishNumbers = traverse (parse snailfishP "") input
  --print snailfishNumbers
  print "hello world"
