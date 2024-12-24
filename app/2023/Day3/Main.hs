import Advent
import Parsers
import Data.Functor

testInput = unlines
  [ "467..114.."
  , "...*......"
  , "..35..633."
  , "......#..."
  , "617*......"
  , ".....+.58."
  , "..592....."
  , "......755."
  , "...$.*...."
  , ".664.598.."
  ]

main :: IO ()
main = do
  input <- readInput 2023 3
  print (part1 testInput)
  pure ()

partNumberP :: Parser Int
partNumberP = symbol *> intP <|> intP <* symbol
  where symbol = symbolChar

--part1 :: String -> Int
part1 = runParser (many ((Just <$> partNumberP) <|> (anySingle $> Nothing)) ) ""
