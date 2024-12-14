import Data.List (foldl')
import Control.Applicative (many)
import qualified Data.Map.Strict as M

buildCharMap :: String -> M.Map Char Int
buildCharMap = foldl' (\m c -> M.insertWith (+) c 1 m) M.empty

ofLetter :: Int -> M.Map Char Int -> Bool
ofLetter n charMap = any (n==) charMap

checkSum :: [String] -> Int
checkSum lines = (length (filter (ofLetter 2) charMaps)) * (length (filter (ofLetter 3) charMaps))
	where charMaps = map buildCharMap lines

diff :: (String, String) -> String
diff ([], []) = []
diff (x:xs, y:ys) = (if x == y then (x:) else id) (diff (xs, ys))

correct :: String -> String -> Bool
correct s1 s2 = length (diff (s1, s2)) == length s1 - 1

main :: IO ()
main = do
	allIDs <- lines <$> readFile "Day02.txt"
	print (checkSum allIDs)
	let corr = [ (s1, s2) | s1 <- allIDs, s2 <- tail allIDs, correct s1 s2 ]
	print corr
	print (map diff corr)
	
	
	
