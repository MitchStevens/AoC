import Data.Maybe
import Data.Monoid
import Text.Parsec
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M

type TowerMap = Map String (Int, [String])
data Tower = Tower String Int [Tower]

type Parser a = Parsec String () a

main :: IO ()
main = do
  file <- readFile "Day07.txt"
  --parseTest (parseTowerMap `sepBy` newline) file
  let Right towerBuilder = parse (parseTowerMap `sepBy` newline) "" file
  let tower = M.fromList towerBuilder
  let bottomProgram = bottom tower
  -- print bottomProgram
  -- let (_, children) = tower ! bottomProgram
  -- print $ map (weight tower) children
  pure ()


parseTag = many letter

parseTowerMap :: Parser (String, (Int, [String]))
parseTowerMap = do
  tag <- parseTag
  weight <- char ' ' *> parseWeight
  holding <- option [] $ do
    string " -> "
    parseTag `sepBy` string ", "
  pure (tag, (weight, holding))

parseWeight :: Parser Int
parseWeight = read <$> (char '(' *> manyTill digit (char ')'))

bottom ::TowerMap -> Int
bottom towerMap = below flippedBuilder (head $ M.elems flippedBuilder)
  where
    below :: Ord a => Map a a -> a -> a
    below m x = maybe x (below m) (M.lookup x m)

    flippedBuilder :: Map String String
    flippedBuilder = M.foldrWithKey go M.empty towerMap where
      go :: String -> (Int, [String]) -> Map String String -> Map String String
      go k (_, vs) m = M.fromList (map (\v -> (v, k)) vs) <> m

buildTower :: TowerMap -> String -> Tower
buildTower tower str = Tower str weight (map (buildTower tower) children)
  where (weight, children) = tower ! str

weight :: Tower -> Int
weight (Tower _ w children) = w + sum (map weight children)