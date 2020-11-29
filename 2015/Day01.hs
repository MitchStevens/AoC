import Data.List (elemIndex)

main :: IO ()
main = do
  parens <- readFile "Day01.txt"
  print $ sum $ map parenValue parens
  print $ elemIndex (-1) $ scanl (+) 0 (map parenValue parens)

parenValue :: Char -> Int
parenValue '(' = 1
parenValue ')' = -1