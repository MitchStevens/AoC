module MonsterMessages where

import Data.IntMap(IntMap, (!))
import qualified Data.IntMap as M
import Debug.Trace
import Control.Monad
import Advent
import Control.Lens
import Control.Monad.State
import Control.Applicative
import Data.Fix
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char
import Data.Foldable
import Data.Char
import Data.List
import Data.Maybe
import Data.Either
import Data.Bifunctor
import Data.Void

type Parser = Parsec Void String

data RuleF a
    = Or [a] [a]
    | And [a]
    | Literal Char
    deriving (Functor, Foldable, Traversable)

instance Show a => Show (RuleF a) where
    show = \case
        Or r1 r2 -> unwords (map show r1) <> " | " <> unwords (map show r2)
        And rs -> unwords . map show $ rs
        Literal c -> ['"', c, '"']

matches :: Fix RuleF -> String -> Bool
matches rule = isJust . parseMaybe (toParserRec rule *> eof)
    where
        toParserRec :: Fix RuleF -> Parser ()
        toParserRec rule = case unFix rule of
            Or r1 r2 -> try (traverse_ toParserRec r1) <|> traverse_ toParserRec r2
            And rs -> traverse_ toParserRec rs
            Literal c -> char c *> pure ()

readRules :: [String] -> IntMap (RuleF Int)
readRules = M.fromList . rights . fmap (parse ruleP "rule")

ruleP :: Parser (Int, RuleF Int)
ruleP = do
    n <- intP <* string ": "
    rule <- choice . map try $
        [ Literal <$> between (char '"') (char '"') (satisfy isAlpha)
        , And <$> manyIntP
        , Or <$> manyTill (intP <* char ' ') (string "| ") <*> manyIntP
        ]
    eof
    pure (n, rule)
    where 
        intP = (read :: String -> Int) <$> takeWhile1P Nothing isDigit
        manyIntP = intP `sepBy1` char ' '

parseInputs :: [String] -> (IntMap (RuleF Int), [String])
parseInputs strs = bimap readRules tail $ span (/="") strs

testInput = 
    [ "0: 4 1 5"
    , "1: 2 3 | 3 2"
    , "2: 4 4 | 5 5"
    , "3: 4 5 | 5 4"
    , "4: \"a\""
    , "5: \"b\""
    , ""
    , "ababbb"
    , "bababa"
    , "abbbab"
    , "aaabbb"
    , "aaaabbb"
    ]

testInput2 =
    [ "42: 9 14 | 10 1"
    , "9: 14 27 | 1 26"
    , "10: 23 14 | 28 1"
    , "1: \"a\""
    , "11: 42 31"
    , "5: 1 14 | 15 1"
    , "19: 14 1 | 14 14"
    , "12: 24 14 | 19 1"
    , "16: 15 1 | 14 14"
    , "31: 14 17 | 1 13"
    , "6: 14 14 | 1 14"
    , "2: 1 24 | 14 4"
    , "0: 8 11"
    , "13: 14 3 | 1 12"
    , "15: 1 | 14"
    , "17: 14 2 | 1 7"
    , "23: 25 1 | 22 14"
    , "28: 16 1"
    , "4: 1 1"
    , "20: 14 14 | 1 15"
    , "3: 5 14 | 16 1"
    , "27: 1 6 | 14 18"
    , "14: \"b\""
    , "21: 14 1 | 1 14"
    , "25: 1 1 | 1 14"
    , "22: 14 14"
    , "8: 42"
    , "26: 14 22 | 1 20"
    , "18: 15 15"
    , "7: 14 5 | 1 21"
    , "24: 14 1"
    , ""
    , "abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa"
    , "bbabbbbaabaabba"
    , "babbbbaabbbbbabbbbbbaabaaabaaa"
    , "aaabbbbbbaaaabaababaabababbabaaabbababababaaa"
    , "bbbbbbbaaaabbbbaaabbabaaa"
    , "bbbababbbbaaaaaaaabbababaaababaabab"
    , "ababaaaaaabaaab"
    , "ababaaaaabbbaba"
    , "baabbaaaabbaaaababbaababb"
    , "abbbbabbbbaaaababbbbbbaaaababb"
    , "aaaaabbaabaaaaababaa"
    , "aaaabbaaaabbaaa"
    , "aaaabbaabbaaaaaaabbbabbbaaabbaabaaa"
    , "babaaabbbaaabaababbaabababaaab"
    , "aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba"
    ]

loopingRules = M.fromList [ (8, Or [42] [42, 8]) , (11, Or [42, 31] [42, 11, 31]) ]

day19 :: IO ()
day19 = do
    let input = testInput2
    --input <- readInput 2020 19
    let (rules, messages) = parseInputs input
    let rule0 = ana (rules !) 0
    print . length . filter (matches rule0) $ messages

    let newRules = M.union loopingRules rules
    let rule0' = ana (newRules !) 0
    traverse_ print . filter (matches rule0') $ messages

