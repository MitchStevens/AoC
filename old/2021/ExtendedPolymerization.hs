{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module ExtendedPolymerization where
import Data.Map (Map)
import Control.Lens
import qualified Data.Map as M
import Data.List.NonEmpty (NonEmpty((:|)), fromList)
import Advent
import Data.Semigroup
import Data.Foldable (traverse_)

type PolymerRules = Map (Char, Char) Char

data Polymer = Polymer
    { initial :: Char
    , pairs :: Map (Char, Char) Integer
    , terminal :: Char }
    deriving (Show)

scale :: Integer -> Polymer -> Polymer
scale n (Polymer initial pairs terminal) = Polymer initial (fmap (n*) pairs) terminal

applyRules :: PolymerRules -> (Char, Char) -> Map (Char, Char) Integer
applyRules rules (e1, e2) = M.fromList . map (,1) $ case M.lookup (e1, e2) rules of
    Nothing -> [(e1, e2)]
    Just p -> [(e1, p), (p, e2)]

applyRulesPolymer :: PolymerRules -> Polymer -> Polymer
applyRulesPolymer rules (Polymer initial pairs terminal) = 
    Polymer initial (M.unionsWith (+) . fmap f $ M.assocs pairs) terminal
    where f (pair, count) = fmap (count *) (applyRules rules pair)

elementCount :: Polymer -> Map Char Integer
elementCount (Polymer initial pairs terminal) = M.fromListWith (+) (M.assocs pairs >>= countPolymer)
    & M.adjust succ initial
    & M.adjust succ terminal
    & fmap (`div` 2)
    where countPolymer ((e1, e2), count) = map (,count) [e1, e2]

score :: Polymer -> Integer
score polymer = maximum instances - minimum instances
    where instances = elementCount polymer

readPolymerRules :: [String] -> (Polymer,  PolymerRules)
readPolymerRules strs = (Polymer initial pairs terminal, rules)
    where
        polyInput = head strs
        (initial, terminal) = (head polyInput, last polyInput)
        pairs = M.fromListWith (+) (zipWith (curry (,1)) polyInput (tail polyInput))
        rules = M.fromList . map readRule $ drop 2 strs
        readRule [c1, c2, _, _, _, _, p] = ((c1, c2), p)

day14 :: IO ()
day14 = do
    (polymer, rules) <- readPolymerRules <$> readInput 2021 14
    print . score $ iterate (applyRulesPolymer rules) polymer !! 10
    print . score $ iterate (applyRulesPolymer rules) polymer !! 40