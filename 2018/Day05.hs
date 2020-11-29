{-# LANGUAGE TupleSections #-}
import Data.Char
import Data.Ord
import Data.List
import Data.Bits

data PolymerStatus = Unreacted | Reacted deriving Show
type Polymer = (PolymerStatus, String)

main :: IO ()
main = do
  file <- readFile "Day05BigBoy.txt"
  print $ length (react file)
  --print $ length (react' file)
  print $ minimum $ map (length . react . flip removeUnit file) ['a'..'z']

removeUnit :: Char -> String -> String
--removeUnit c = filter (`notElem` [c, toUpper c])
removeUnit c = filter (\c' -> 0 == 16 .&. (xor (ord c) (ord c')))

reactsWith :: Char -> Char -> Bool
reactsWith a b = 32 == xor (ord a) (ord b)

react :: String -> String
react = snd . reactPolymer . (Unreacted,)

reactPolymer :: Polymer -> Polymer
reactPolymer (Unreacted, x:xs) = case reactPolymer (Unreacted, xs) of
    (Reacted, y:zs) -> if x `reactsWith` y then (Reacted, zs) else (Reacted, x:y:zs)
    _ -> (Reacted, [x])
reactPolymer (_, polymer) = (Reacted, polymer)

react' :: String -> String
react' (x:xs) = case react' xs of
    y:zs-> if x `reactsWith` y then react' zs else x:y:zs
    _ -> [x]
react' _ = ""

