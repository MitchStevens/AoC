{-# OPTIONS_GHC -Wno-type-defaults #-}
module JurassicJigsaw where
import Data.List.Split (splitOn)
import Data.Map (Map, (!))
import qualified Data.Map as M
import Advent (readInput)
import Point hiding (up, down, left, right)
import Data.Traversable (for)
import Data.Set (Set, (\\))

type TileId = Int

data Tile = Tile
    { upEdge :: String -- left to Right
    , rightEdge :: String -- up to down
    , downEdge :: String -- right to left
    , leftEdge :: String -- down to up
    }

type Image = Map Point (Maybe TileId)

makeTile :: [String] -> Tile
makeTile strs = Tile up right down left
    where
        up = head strs
        right = map last strs
        down = reverse (last strs)
        left = reverse (map head strs)
    
rotationGroup :: Tile -> [Tile]
rotationGroup tile = allRotations tile <> allRotations (leftRightReflection tile)
    where
        allRotations = take 4 . iterate undefined --rotateClockWise
        rotateClockwise (Tile up right down left) = Tile left up right down
        leftRightReflection (Tile up right down left) = Tile (reverse right) (reverse up) (reverse left) (reverse down)

leftRightMatch :: Tile -> Tile -> Bool
leftRightMatch t1 t2 = rightEdge t1 == reverse (leftEdge t2)

tileMatchesAtPosition :: Map TileId Tile -> Image -> Tile -> Point -> [Image]
tileMatchesAtPosition m image tile point = undefined--rotationGroup

tileMatches :: Map TileId Tile -> Image -> Tile -> [Image]
tileMatches m image tile = pointsAdjacentToTiles >>= tileMatchesAtPosition m image tile
    where
        pointsAdjacentToTiles = undefined --S.fromList (M.elems image >>= surrounding) \\ S.fromList (M.elems image)

        

parseTile :: [String] -> (TileId, Tile)
parseTile strs = (tileId, makeTile (tail strs))
    where tileId = read $ init (drop 5 (head strs))


day20 :: IO ()
day20 = do
    input <- readInput 2020 201
    let tiles = M.fromList . map parseTile . splitOn [""] $ input
    let (n :: Int) = floor (sqrt (fromIntegral (length tiles)))
    let emptyImage = M.empty
    print (length tiles)




