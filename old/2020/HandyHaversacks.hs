{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module HandyHaversacks where

import Advent
import Control.Comonad.Cofree
import Control.Applicative
import Data.Fix
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Data.Foldable
import Data.Char
import Data.Functor
import Data.Map (Map, (!))
import qualified Data.Map as M
import Data.Either
import Data.Void

type Parser = Parsec Void String

data Bag = Bag { quality :: String, color :: String }
    deriving (Eq, Ord, Show)

newtype ContainsF a = Contains { getContains :: [(Int, a)] }
    deriving (Show, Functor, Foldable)


bagP :: Parser Bag
bagP = do
    quality <- word <* space
    color <- word <* space
    string "bag"
    optional (char 's')
    pure (Bag quality color)
    where word = takeWhile1P Nothing isAlpha

bagRuleP :: Parser (Bag, ContainsF Bag)
bagRuleP = do
    bag <- bagP
    string " contain "
    contains <- fmap Contains $
        try (string "no other bags" $> [])
            <|> (containsP `sepBy1` string ", ")
    char '.' <* eof
    pure (bag, contains)
    where
        containsP = (,) <$> (intP <* space) <*> bagP
        intP = (read :: String -> Int) <$> takeWhile1P Nothing isDigit

shinyGoldBag :: Bag
shinyGoldBag = Bag "shiny" "gold"

containsShinyGoldBag :: Map Bag (ContainsF Bag) -> Bag -> Bool
containsShinyGoldBag m bag = (bag /= shinyGoldBag) && elem shinyGoldBag (coiter (m!) bag)

countBags :: Map Bag (ContainsF Bag) -> Bag -> Int
countBags m = pred . hylo (succ . f) (m !)
    where f  = sum . map (uncurry (*)) . getContains

testInput =
    [ "light red bags contain 1 bright white bag, 2 muted yellow bags."
    , "dark orange bags contain 3 bright white bags, 4 muted yellow bags."
    , "bright white bags contain 1 shiny gold bag."
    , "muted yellow bags contain 2 shiny gold bags, 9 faded blue bags."
    , "shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags."
    , "dark olive bags contain 3 faded blue bags, 4 dotted black bags."
    , "vibrant plum bags contain 5 faded blue bags, 6 dotted black bags."
    , "faded blue bags contain no other bags."
    , "dotted black bags contain no other bags."
    ]

test2 =
    [ "shiny gold bags contain 2 dark red bags."
    , "dark red bags contain 2 dark orange bags."
    , "dark orange bags contain 2 dark yellow bags."
    , "dark yellow bags contain 2 dark green bags."
    , "dark green bags contain 2 dark blue bags."
    , "dark blue bags contain 2 dark violet bags."
    , "dark violet bags contain no other bags."
    ]

day7 :: IO ()
day7 = do
   --let input = test2
   input <- readInput 2020 7
   let bagRules = M.fromList . rights . map (parse bagRuleP "") $ input
   print . length $ filter (containsShinyGoldBag bagRules) (M.keys bagRules)
   print (countBags bagRules shinyGoldBag)

