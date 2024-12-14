import Text.Parsec
import Data.IntMap (IntMap, (!?))
import qualified Data.IntMap as M
import Data.Maybe
import Control.Applicative (liftA2)
import Data.List

data Tree = Node [Tree] [Int] deriving Show

main :: IO ()
main = do
  file <- readFile "Day08.txt"
  let Right tree = parse parseTree "" file
  --print tree
  print $ sumMetadata tree
  print $ nodeValue tree

parseInt = read <$> (many digit)

parseTree :: Parsec String () Tree
parseTree = do
  numChildren <- parseInt
  space
  numMetaData <- parseInt
  children <- count numChildren (space *> parseTree)
  metaData <- count numMetaData (space *> parseInt)
  pure $ Node children metaData

sumMetadata :: Tree -> Int
sumMetadata (Node subtrees metadata) = sum metadata + sum (map sumMetadata subtrees)

nodeValue :: Tree -> Int
nodeValue (Node [] metadata) = sum metadata
nodeValue (Node subtrees metadata) = sum $ map (\i -> fromMaybe 0 (m !? i)) metadata
  where m = M.fromList $ zip [1..] (map nodeValue subtrees)