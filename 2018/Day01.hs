import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad

readInt :: String -> Int
readInt str = case str of
  '+':s -> read s
  '-':s -> -(read s)
  _ -> undefined

findTwice :: Set Int -> Int -> Either Int (Set Int)
findTwice set x = if x `S.member` set then Left x else Right (S.insert x set)

main :: IO ()
main = do
  numStrings <- lines <$> readFile "Day01.txt"
  let nums = map readInt numStrings
  print $ sum nums
  let freqList = scanl (+) 0 (cycle nums)
  print $ foldM findTwice S.empty freqList
