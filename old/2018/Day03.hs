import Data.Monoid
import Data.List (foldl', elemIndex, find)
import Data.Map.Lazy (Map, (!))
import qualified Data.Map.Lazy as M
import Control.Monad
import Text.Parsec
import Data.Ix
import Data.Maybe

type Parser a = Parsec String () a

data Claim = Claim Int (Int, Int) (Int, Int) deriving (Eq, Show)

main :: IO ()
main = do
  file <- readFile "Day03.txt"
  let Right claimList = parse (parseClaim `sepBy` newline) "" file
  let claimMap = foldl' addClaim (M.empty) claimList
  print $ length $ M.filter (>1) claimMap
  print $ filter (noOverlap claimMap) claimList
  print $ 1 + (fromJust . disjoint) claimList

parseInt = read <$> (many digit)

claimArea :: Claim -> [(Int, Int)]
claimArea (Claim _ p1 p2) = range (p1, p2)

parseClaim :: Parser Claim
parseClaim = do
  id <- string "#" *> parseInt <* string " @ "
  x <- parseInt <* char ','
  y <- parseInt <* string ": "
  w <- parseInt <* string "x"
  h <- parseInt
  pure $ Claim id (x, y) (x+w-1, y+h-1)

addClaim :: Map (Int, Int) Int -> Claim -> Map (Int, Int) Int
addClaim m claim = M.unionWith (+) m $ M.fromList (map (\x -> (x, 1)) (claimArea claim))

noOverlap :: Map (Int, Int) Int -> Claim -> Bool
noOverlap m claim = all  (\x -> m ! x == 1 ) (claimArea claim)

meet :: Claim -> Claim -> Bool
meet (Claim _ (x, y) (w,z)) (Claim _ (a, b) (c, d)) = max x a <= min w c && 
                                     max y b <= min z d

disjoint :: [Claim] -> Maybe Int
disjoint xs = find p xs >>= flip elemIndex xs
  where p x = filter (meet x) xs == [x]