module LobbyLayout where

import Point
import Advent
import Data.Set (Set)
import Data.Foldable
import Data.Functor.Identity
import qualified Data.Set as S
import Control.Monad.State
import Control.Monad.State.Class

hexAdjacent :: Point -> [Point]
hexAdjacent point = map ($ point) [right, down . left, down, left, up, up . right]

parseHexDirections :: String -> Point
parseHexDirections [] = origin
parseHexDirections ('e':    cs) = right $ parseHexDirections cs
parseHexDirections ('s':'w':cs) = down . left $ parseHexDirections cs
parseHexDirections ('s':'e':cs) = down $ parseHexDirections cs
parseHexDirections ('w':    cs) = left $ parseHexDirections cs
parseHexDirections ('n':'w':cs) = up $ parseHexDirections cs
parseHexDirections ('n':'e':cs) = up . right $ parseHexDirections cs

isBlack :: (MonadState (Set Point) m) => Point -> m Bool
isBlack point = gets (S.member point)

toggleTile :: (MonadState (Set Point) m) => Point -> m ()
toggleTile p = modify (runIdentity . S.alterF (Identity . not) p)

nextStatePoint :: (MonadState (Set Point) m) => Point -> m ()
nextStatePoint point = do
    adjBlack <- length . filter id <$> traverse isBlack (hexAdjacent point)
    black <- isBlack point
    case black of
        True  -> when (adjBlack == 0 || adjBlack > 2) $ toggleTile point
        False -> when (adjBlack == 2) $ toggleTile point

nextState :: (MonadState (Set Point) m) => m ()
nextState = do
    (blackTiles :: Set Point) <- get
    traverse_ nextStatePoint (blackTiles <> foldMap (S.fromList . hexAdjacent) blackTiles)

testInput =
    [ "sesenwnenenewseeswwswswwnenewsewsw"
    , "neeenesenwnwwswnenewnwwsewnenwseswesw"
    , "seswneswswsenwwnwse"
    , "nwnwneseeswswnenewneswwnewseswneseene"
    , "swweswneswnenwsewnwneneseenw"
    , "eesenwseswswnenwswnwnwsewwnwsene"
    , "sewnenenenesenwsewnenwwwse"
    , "wenwwweseeeweswwwnwwe"
    , "wsweesenenewnwwnwsenewsenwwsesesenwne"
    , "neeswseenwwswnwswswnw"
    , "nenwswwsewswnenenewsenwsenwnesesenew"
    , "enewnwewneswsewnwswenweswnenwsenwsw"
    , "sweneswneswneneenwnewenewwneswswnese"
    , "swwesenesewenwneswnwwneseswwne"
    , "enesenwswwswneneswsenwnewswseenwsese"
    , "wnwnesenesenenwwnenwsewesewsesesew"
    , "nenewswnwewswnenesenwnesewesw"
    , "eneswnwswnwsenenwnwnwwseeswneewsenese"
    , "neswnwewnwnwseenwseesewsenwsweewe"
    , "wseweeenwnesenwwwswnew"
    ]

day24 :: IO ()
day24 = do
    let input = testInput
    --input <- readInput 2020 24
    let tiles = map parseHexDirections input
    print . length . (`execState` S.empty) $ traverse_ toggleTile tiles
    print . length . (`execState` S.empty) $ do
        traverse_ toggleTile tiles
        nextState
