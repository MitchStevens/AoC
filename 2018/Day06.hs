import Data.Ord
import Data.List
import Data.List
import Data.Monoid
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
import Data.Geometry

main :: IO ()
main = do
  file <- readFile "Day06.txt"
  let points = readPoint <$> lines file
  let polygon
  

readPoint :: String -> Point 2 Double
readPoint str = point2 (n!!0) (n!!1)
  where n = read <$> (words $ filter (','/=) str)

manhattan :: Point -> Point -> Int
manhattan (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)



getClosestIndex :: [Point] -> Point -> Maybe Int
getClosestIndex ps p = if dist i < dist j then Just i else Nothing
   where
      i:j:_ = sortBy (comparing dist) [0..(length ps -1)]
      dist i = manhattan p (ps!!i)