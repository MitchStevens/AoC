import Data.List
import Text.Parsec (Parsec, parseTest, newline, many, digit, char, sepBy, parse)

main :: IO ()
main = do
  file <- readFile "day02.txt"
  let Right volumes = parse (parseArea `sepBy` newline) "" file
  print (sum $ map totalArea volumes)
  print (sum $ map ribbon volumes)
  pure ()

type Parser a = Parsec String () a

parseArea :: Parser (Int, Int, Int)
parseArea = do
  x <- read <$> many digit
  char 'x'
  y <- read <$> many digit
  char 'x'
  z <- read <$> many digit
  pure (x, y, z)

totalArea :: (Int, Int, Int) -> Int
totalArea (x, y, z) = 2 * (x*y + y*z + x*z) + minimum [x*y, x*z, y*z]

ribbon :: (Int, Int, Int) -> Int
ribbon (x, y, z) = 2 * (sum $ take 2 (sort [x, y, z])) + (x*y*z)