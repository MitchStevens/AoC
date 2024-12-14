module SonarSweep where

import Advent

day1 :: IO ()
day1 = do
  measurements <- map (read :: String -> Int) <$> readInput 2021 1
  print . length . filter id $ zipWith (<) measurements (tail measurements)
  print . length . filter id $ zipWith (<) measurements (drop 3 measurements)
