module Day20 where

data Tile =
  Quad [[Tile]]

class Tile t where
  makeQuad :: t -> t -> t -> t ->[Quad]
  rotations :: t -> [t]

makeQuad

solve :: Tile t => Int -> Int -> [t] -> [[Tile]]
solve w h tiles =






createTile :: [String] -> Tile

rotations :: Tile -> [Tile]
rotations tile


matches :: Tile -> Tile -> [(Tile, Tile)]
matches tile tile
