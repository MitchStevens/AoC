import Advent
import Parsers
import Data.Functor
import qualified Data.Set as S
import Control.Monad.Memo

testInput = unlines
  [ "r, wr, b, g, bwu, rb, gb, br"
  , ""
  , "brwrr"
  , "bggr"
  , "gbbr"
  , "rrbgbr"
  , "ubwu"
  , "bwurrg"
  , "brgr"
  , "bbrgwb"
  ]

data Colour = W | U | B | R | G
  deriving (Eq, Ord, Show)

type TowelPattern = [Colour]
type Design = [Colour]

colourP :: Parser Colour
colourP = choice
  [ char 'w' $> W
  , char 'u' $> U
  , char 'b' $> B
  , char 'r' $> R
  , char 'g' $> G
  ]

inputP :: Parser ([TowelPattern], [Design])
inputP = do
  towelPatterns <- (some colourP) `sepBy` chunk ", " <* newline
  newline
  designs <- (some colourP) `sepEndBy` newline
  pure (towelPatterns, designs)

main :: IO ()
main = do
  input <- readInput 2024 19
  let Right (towelPatterns, designs) = runParser inputP "" input
  print (part1 towelPatterns designs)
  print (part2 towelPatterns designs)

arrangementCount :: [TowelPattern] -> Design -> Int
arrangementCount towelPatterns design = startEvalMemo (countMemo design)
  where
    countMemo :: Design -> Memo Design Int Int
    countMemo [] = pure 1
    countMemo xs = do
      let designs = S.toList . S.fromList . map (\p -> drop (length p) xs) . filter (`isPrefixOf` xs) $ towelPatterns
      sum <$> traverse (memo countMemo) designs

part1 :: [TowelPattern] -> [Design] -> Int
part1 towelPatterns = length . filter (> 0) . map (arrangementCount towelPatterns)

part2 :: [TowelPattern] -> [Design] -> Int
part2 towelPatterns = sum . map (arrangementCount towelPatterns)
