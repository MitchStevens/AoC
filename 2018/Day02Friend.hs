--I dont think this compiles anymore btw

main = do
    input <- readFile "input"
    print $ soln1 input
    print $ soln2 input

soln1 :: [String] -> Int
soln1 s = countTwos x * countThrees x
  where x = countLetters (lines s)

soln2 :: String -> String
soln2 x = findCommon (findDiff1 (lines x) (lines x))

--Using a Map instead of a [(Char, Int)] makes this function very short
countLetters :: String -> Map Char Int
countLetters s = foldl' (\m c -> Map.insertWith (+) c 1 m) Map.empty

--Very easy to generalise to hasThree
hasTwo :: Map Char Int -> Bool
hasTwo = any (2==)

countTwos :: Map Char Int -> Int
countTwos = Map.length . Map.filter hasTwo . countLetters

--There is a function that does zip and map alreadly included, `zipWith`
countDiff :: String -> String -> Int
coundDiff s0 s1 = sum $ zipWith (\a b -> if a == b then 0 else 1) s0 s1

findDiff1 :: [String] -> [String] -> (String, String)c
findDiff1 [] [] = ("", "")
findDiff1 (x:xs) [] = findDiff1 xs xs
findDiff1 (x:xs) (y:ys) = if (countDiff x y) == 1 then (x, y) else findDiff1 (x:xs) ys

--Use pattern matching to improve readablity
findCommon :: (String, String) -> String
findCommon ([], []) = []
findCommon (x:xs) (y:ys)
    | x == y    = x: findCommon xs ys
    | otherwise = findCommon xs ys

--Although i suspoect you actaully want to use `zipWith` again
findCommon :: (String, String) -> String
findCommon s0 s1 = concat (zipWith (\a b -> if a == b then [a] else []))