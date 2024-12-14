{-# LANGUAGE OverloadedStrings #-}

import Advent
import qualified Data.Array as A
import Data.List
import Control.Monad
import Array2D
import Point
import Data.Maybe
import Data.Ix
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

data BlockType
  = File { fileId :: Int }
  | Empty

type Block = Block { blockType :: BlockType, size :: Int }

isFile :: Block -> Maybe Int
isFile block = case blockType of
  File fileId -> Just fileId
  _ -> Nothing

type Filesystem = [Block]

testInput = "2333133121414131402"

main = do
  input <- lines <$> readInput 2024 9

parseFilesystem :: String -> Filesystem
parseFilesystem = parseFilesystem 0
  where
    parseFilesystemRec :: Int -> String -> Filesystem
    parseFilesystemRec n str = case str of
      x:y:zs -> Block (File n) (read [x]) : Block Empty (read [y]) : parseFilesystemRec (n+1) zs
      _ -> []
    
insertBlock :: Block -> Filesystem -> Filesystem
insertBlock (Block Empty _) fs = fs
insertBlock (Block (File fileId) size) fs = case fs of
  Block Empty n : xs -> case compare size n of
    LT -> Block (File fileId) size : Block Empty (n-size) : fs
    EQ -> Block (File fileId) size : fs
    GT -> Block (File fileId) n : insertBlock (Block (File fileId) (size-n))
  Block (File fileId') n : xs -> Block (File fileId') n : insertBlock (Block (File fileId) size) xs
insertBlock block [] = block

moveFileBlocks :: Filesystem -> Filesystem
moveFileBlocks fs = case fileMoveComplete fs of
  Just (block, fs') -> moveFileBlocks (insertBlock block fs')
  Nothing -> fs
  where
    fileMoveComplete :: Maybe (Block, FileSystem)
    fileMoveComplete =
      if all (not . isFile) (dropWhile isFile fs)
        then getRightMostBlock
        else Nothing
    
    getRightMostBlock :: Maybe (Block, Filesystem)
    getRightMostBlock = do
      fileBlock <- find (isJust . isFile) (reverse fs)
      let fs' = map (\b@(Block _ size) -> if b == fileBlock then Block Empty size else b) fs 
      pure ()


removeLastFile :: Filesystem -> Maybe (Block, Filesystem)
removeLastFile 

compactFilesystem :: Filesystem -> Filesystem
compactFilesystem fs = case fs of
  Block Empty x: Block Empty y : zs -> compactFilesystem $ Block Empty (x+y) : zs
  _ -> fs


part1 :: Map Char (Set Point) -> (Point -> Bool) -> Int
part1 nodes inBounds = S.size . S.filter inBounds $ foldMap findAntinodes nodes
  
  
part2 :: Map Char (Set Point) -> (Point -> Bool) -> Int
part2 nodes inBounds = S.size . S.filter inBounds $ foldMap (findTrueAntinodes inBounds) nodes