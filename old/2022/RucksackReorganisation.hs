{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module RucksackReorganisation where
import Advent (readInput)
import Data.Bifunctor (Bifunctor(bimap))
import Data.List (intersect, sort, nub)
import Data.Char (isLower)
import Data.Foldable (traverse_)

toRucksack :: String -> (String, String)
toRucksack str = bimap (sort . nub) (sort . nub) $ splitAt (length str `div` 2) str

priority :: Char -> Int
priority c =
    if isLower c
        then 1 + fromEnum c - fromEnum 'a'
        else 27 + fromEnum c - fromEnum 'A'

triplets :: [a] -> [[a]]
triplets [] = []
triplets xs = take 3 xs : triplets (drop 3 xs)

intersects :: Eq a => [[a]] -> [a]
intersects xs = foldr intersect (head xs) (tail xs)

day3 :: IO ()
day3 = do
    input <- readInput 2022 3
    print . sum . map (sum . map priority . uncurry intersect . toRucksack) $ input
    print . sum . map ((sum . map priority) . intersects) . triplets . map nub $ input