import Advent
import Point
import Facing
import Data.Ix
import Data.Set (Set)
import qualified Data.Set as S
import Array2D as A2D
import Algebra.Graph
import Algebra.Graph.ToGraph
import Data.Array as A
import qualified Data.Map as M
import Data.Monoid
import Data.Tree as T
import Data.Foldable
import Zipper

type Fence = [(Point, Point)]

testInput =
  [ "RRRRIICCFF"
  , "RRRRIICCCF"
  , "VVRRRCCFFF"
  , "VVRCCCJFFF"
  , "VVVVCJJCFE"
  , "VVIVCCJJEE"
  , "VVIIICJJEE"
  , "MIIIIIJJEE"
  , "MIIISIJEEE"
  , "MMMISSJEEE"
  ]



cost :: Set Point -> Int
cost region = area * perimeter
  where
    area = length region
    perimeter

simplifyFenceAt :: CycleZipper Point -> Maybe Point
simplifyFenceAt cz = 
  where
    isLine :: f Point -> Bool
    isLine points = 


simplifyFence :: Fence -> Fence
simplifyFence [] = []
simplifyFence toList 
  where
    m = M.fromList fence

    

    mergeFences :: Point -> Point -> Point -> Fence
    mergeFences p1 p2 p3 =
      if
    
      
      Map Point Point -> Map Point Point
    mergeFences p = fromMaybe id $ do
      p1 <-




fenceAround :: Set Point -> Point -> Fence
regionStatsAround region p = do
  p' <- cardinal p
  []
  let n = length . filter (`S.member` region) $ 
  in RegionStats 1 (4-n)

regionStats :: Set Point -> Fence
regionStats points = foldMap (regionStatsAround points) points

--getConnectedPlants :: Array Point Char -> Point -> Set Point
--getConnectedPlants plantMap p = getConnectedPlantsRec (S.singleton p) S.empty
--  where
--    getConnectedPlantsRec :: Set Point -> Set Point -> Set Point
--    getConnectedPlantsRec open connected = 
--      if S.null open
--        then connected
--        else
--          let S.map (plantMap ! ) (cardinal )

buildPlantGraph :: Array Point Char -> Graph (Point, Char)
buildPlantGraph array2D = stars $ do
  p <- range (A.bounds array2D)
  let c = array2D ! p
  let ps = filter (inRange (A.bounds array2D)) (cardinal p)
  let leaves = map (\p' -> (p', array2D ! p')) ps
  pure ((p, c), filter ((c==) . snd) leaves)

main :: IO ()
main = do
  input <- lines <$> readInput 2024 12
  let plantGraph = buildPlantGraph (readArray2D id input)
  print (part1 plantGraph)

part1 plantGraph = sum $ map (cost . regionStats) plantRegions
  where
    plantRegions :: [Set Point]
    plantRegions = map (S.fromList . map fst . toList) (dfsForest plantGraph)



