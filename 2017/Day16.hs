import Control.Applicative hiding (many)
import Data.Foldable
import Data.Traversable
import Control.Monad
import Data.Maybe
import Data.List
import Data.Monoid
import Text.Parsec
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

data Move = Spin Int | Exchange Int Int | Partner Char Char deriving Show
type Programs = Seq Char

main :: IO ()
main = do
  file <- readFile "Day16.txt"
  let Right moves = parse (parseMove `sepBy` char ',') "" file
  print $ foldl' move (Seq.fromList ['a'..'p']) moves
  traverse_ print $ take 20 $ scanl (foldl' move) (Seq.fromList ['a'..'p']) (repeat moves)

parseInt = read <$> (many digit)

parseMove = choice
  [ Spin <$> (char 's' *> parseInt)
  , Exchange <$> (char 'x' *> parseInt) <*> (char '/' *> parseInt)
  , Partner  <$> (char 'p' *> letter) <*> (char '/' *> letter)
  ]

move :: Programs -> Move -> Programs
move ps m = case m of
  Spin s -> Seq.drop s' ps <> Seq.take s' ps
    where s' = Seq.length ps - s
  Exchange i j -> fromJust $ do
    a <- Seq.lookup i ps
    b <- Seq.lookup j ps
    pure $ Seq.update i b . Seq.update j a $ ps
  Partner a b -> fromJust $ do
    i <- Seq.elemIndexL a ps
    j <- Seq.elemIndexL b ps
    pure $ Seq.update i b . Seq.update j a $ ps
