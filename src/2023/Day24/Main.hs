{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeApplications #-}

import Advent
import Parsers hiding ((<|>))
import Data.Matrix
import Data.Maybe
import Data.Ix
import Data.Vector (Vector)
import Data.Foldable
import qualified Data.Vector as V
import Data.SBV as SBV
import Data.Int

testInput = unlines
  [ "19, 13, 30 @ -2,  1, -2"
  , "18, 19, 22 @ -1, -1, -2"
  , "20, 25, 34 @ -2, -2, -4"
  , "12, 31, 28 @ -1, -2, -1"
  , "20, 19, 15 @  1, -5, -3"
  ]

type Point3 = [Int64]

data Hailstone = Hailstone
  { pos :: Point3
  , vel :: Point3
  }
  deriving (Eq, Ord, Show)

main :: IO ()
main = do
  input <- readInput 2023 24
  let Right hailstones = runParser (hailstoneP `sepEndBy` newline) "" testInput
  --part1 hailstones >>= print
  part2 hailstones >>= print

point3P :: Parser Point3
point3P = do
  x <- intP <* chunk "," <* space 
  y <- intP <* chunk "," <* space 
  z <- intP
  pure (fmap fromIntegral [x, y, z])

hailstoneP :: Parser Hailstone
hailstoneP = do
  pos <- point3P <* space <* char '@' <* space
  vel <- point3P
  pure (Hailstone pos vel)

intersection2 :: Hailstone -> Hailstone -> IO Bool
intersection2 h1 h2 = fmap modelExists . sat $ do
  s <- sInt64 "s"
  t <- sInt64 "t"
  constrain $ s .>= 0
  constrain $ t .>= 0
  for_ (take 2 $ zip4 (pos h1) (vel h1) (pos h2) (vel h2)) $ \(p1, v1, p2, v2) -> do
    let l1 = fromIntegral p1 + s*(fromIntegral v1)
    let l2  = fromIntegral p2 + t*(fromIntegral v2)
    constrain $ l1 .== l2
    constrain $ a .<= l1
    constrain $ l1 .<= b

  where
    (a, b) = (fromIntegral 200000000000000, fromIntegral 400000000000000)
    --(a, b) = (fromIntegral 7, fromIntegral 27)




part1 :: [Hailstone] -> IO Int
part1 hailstones = fmap length $ filterM (uncurry intersection2)
  (distinctPairs hailstones)
  
-- p1 + t*v1 = pr + t*vr

part2 :: [Hailstone] -> IO Int64
part2 hailstones = do
  --res <- satWith (cvc5 {extraArgs = [ "-memory:4000" ]} ) symbolics
  res <- satWith mathSAT symbolics
  --res <- prove z3 symbolics
  --let optimizeStyle = Lexicographic
  --res <- dsatWith (z3 {extraArgs = [ "-memory:4000" ]} ) symbolics


  print ( res)
  pure 0

  where


    symbolics :: Symbolic ()
    symbolics = do
      prx <- sInt64 "prx"
      pry <- sInt64 "pry"
      prz <- sInt64 "prz"
      vrx <- sInt64 "vrx"
      vry <- sInt64 "vry"
      vrz <- sInt64 "vrz"
      let pr = [ prx, pry, prz ]
      let vr = [ vrx, vry, vrz ]

      for_ hailstones $ \h -> do
        lambda <- sInt64_
        constrain $ lambda .>= 0
        for_ (zip4 (pos h) (vel h) pr vr) $ \(p, v, p', v') -> do
          let l1 = fromIntegral p + lambda*(fromIntegral v)
          let l2  = p' + lambda * v'
          constrain $ l1 .== l2