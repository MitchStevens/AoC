module DumboOctopus where

import Point
import Control.Monad.State.Strict
import Advent
import Data.Array (Array, (!), indices, listArray)
import Data.Foldable (traverse_)
import Control.Lens hiding (indices)
import Data.Semigroup (Sum)
import Control.Monad.Writer (MonadWriter (tell), runWriter, execWriter)
import Data.Char (digitToInt)

increment :: (MonadState (Array Point Int) m, MonadWriter (Sum Int) m) => Point -> m ()
increment p = do
    b <- gets (has (ix p))
    when b $ do
        ix p += 1
        power <- gets (! p)
        when (power == 10) $ do
            tell 1 
            forM_ (adjacent p) $ \adj -> do
                increment adj

gameTick :: (MonadState (Array Point Int) m, MonadWriter (Sum Int) m) => m ()
gameTick = do
    octopi <- gets indices
    traverse_ increment octopi
    traverse %= (\power -> if power > 9 then 0 else power)

firstAllFlash :: (MonadState (Array Point Int) m, MonadWriter (Sum Int) m) => m Int
firstAllFlash = do
    gameTick
    allFlash <- gets (all (==0))
    if allFlash then pure 1 else succ <$> firstAllFlash

readOctopi :: [[Char]] -> Array Point Int
readOctopi strs = listArray ((0, 0), top) (strs >>= map digitToInt)
    where top = (length strs -1, length (head strs) -1)

test1 = [ "5483143223" , "2745854711" , "5264556173" , "6141336146" , "6357385478"
        , "4167524645" , "2176841721" , "6882881134" , "4846848554" , "5283751526" ]

test2 = [ "11111" , "19991" , "19191" , "19991" , "11111" ]

day11 :: IO ()
day11 = do
    --let octopi = readOctopi test1
    octopi <- readOctopi <$> readInput 2021 11
    --let f n = if n > 9 then '*' else head (show n)
    print . execWriter . flip evalStateT octopi $ replicateM_ 100 gameTick
    print . fst . runWriter . flip evalStateT octopi $ firstAllFlash