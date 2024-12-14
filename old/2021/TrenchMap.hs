{-# OPTIONS_GHC -Wno-name-shadowing #-}
module TrenchMap where

import Point 
import Data.Map.Strict (Map, (!))
import Advent (readInput)
import qualified Data.Map as M
import Binary
import Data.Maybe
import Control.Monad

type Rules = Map Int Bool
data InfiniteImage = InfiniteImage
    { defaultPixel :: Bool
    , finiteImage :: Map Point Bool
    }

getPixel :: InfiniteImage -> Point -> Bool
getPixel (InfiniteImage def finiteImage) p = fromMaybe def (M.lookup p finiteImage)

stepPoint :: Rules -> InfiniteImage -> Point -> Bool
stepPoint rules image point = rules ! n
    where n = binaryToInt . Binary . reverse . map (getPixel image) $ surrounding point

step :: Rules -> InfiniteImage -> InfiniteImage
step rules image@(InfiniteImage def finiteImage) = InfiniteImage def' finiteImage'
    where
        relevantPixels =  M.keys finiteImage >>= surrounding
        def' = if def then rules ! 512 else rules ! 0
        finiteImage' = M.fromList . map (\p -> (p, stepPoint rules image p)) $ relevantPixels

step' :: Rules -> InfiniteImage -> Map Point Int
step' rules image@(InfiniteImage def finiteImage) = finiteImage'
    where
        relevantPixels =  M.keys finiteImage
        finiteImage' = M.fromList . map (\p -> (p, f p)) $ relevantPixels
        f p = binaryToInt . Binary . reverse . map (getPixel image) $ surrounding p

pixelsLit :: InfiniteImage -> Int
pixelsLit (InfiniteImage def finiteImage) = (if def then (maxBound :: Int) else 0) + length (M.filter id finiteImage)

parseInput :: [String] -> (Rules, InfiniteImage)
parseInput strs = (M.fromList . zip [0..] . map ('#'==) $ head strs, InfiniteImage False image )
    where
        imageInput = drop 2 strs
        (m, n) = (length (head imageInput), length imageInput)
        image = M.fromList $ do
            i <- [0..(m-1)]
            j <- [0..(n-1)]
            pure ((i, j), ((imageInput !! j) !! i) == '#')

testImage image =
    forM_ [-5..5] $ \j -> do
        forM_ [-5..5] $ \i ->
            putChar (if getPixel image (i, j) then '#' else '.')
        putChar '\n'

day20 :: IO ()
day20 = do
    (rules, image) <- parseInput <$> readInput 2021 200
    testImage $ image
    print ""
    testImage . step rules $ image
    print . step' rules $ image
