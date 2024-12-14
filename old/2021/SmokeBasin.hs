{-# OPTIONS_GHC -Wno-name-shadowing #-}
module SmokeBasin where

import Advent (readInput)
import Point
import Data.Array
import Data.Char (digitToInt)
import Data.Set (Set)
import Control.Monad.State.Strict
import qualified Data.Set as S
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.RWS (evalRWS)
import Data.List (sort)

notPeak :: Array Point Int -> Point -> Bool
notPeak floor point = floor ! point /= 9

safeCardinal :: Array Point Int -> Point -> [Point]
safeCardinal floor point = filter (inRange (bounds floor)) (cardinal point)

lowPoints :: Array Point Int -> [Point]
lowPoints floor = filter (notPeak floor) . filter isLow $ indices floor
    where isLow point = all (\adj -> floor ! point < floor ! adj) (safeCardinal floor point)

singleBasin :: (MonadReader (Array Point Int) m, MonadState (Set Point) m) => Point -> m [Point]
singleBasin point = do
    notInClosedSet <- gets (S.notMember point)
    notPeak <- asks notPeak <*> pure point
    if notInClosedSet && notPeak
        then do
            modify (S.insert point)
            adjs <- asks safeCardinal <*> pure point
            (point:) . mconcat <$> traverse singleBasin adjs
        else
            pure []

allBasins :: (MonadReader (Array Point Int) m, MonadState (Set Point) m, MonadWriter [[Point]] m) => m ()
allBasins = do
    points <- asks lowPoints 
    forM_ points $ \p -> do
        notInClosedSet <- gets (S.notMember p)
        when notInClosedSet (singleBasin p >>= (tell . pure))

readFloor :: [String] -> Array Point Int
readFloor strs = listArray ((0, 0), top) (strs >>= map digitToInt)
    where top = (length strs -1, length (head strs) -1)

testFloor = readFloor
    [ "2199943210"
    , "3987894921"
    , "9856789892"
    , "8767896789"
    , "9899965678"
    ]

day9 :: IO ()
day9 = do
    --let floor = testFloor
    floor <- readFloor <$> readInput 2021 9
    print . sum . map (succ . (floor !)) . lowPoints $ floor
    print . product . take 3 . reverse . sort . map length . snd $ evalRWS allBasins floor S.empty
    