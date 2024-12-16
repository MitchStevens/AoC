import Advent

import Data.MultiSet (MultiSet)
import qualified Data.MultiSet as MS
import Parsers

testInput = unlines
  [ "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
  , "Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue"
  , "Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red"
  , "Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
  , "Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"
  ]

type Observation = MultiSet String

data Game = Game
  { gameId :: Int
  , observations :: [Observation]
  }
  deriving (Eq, Show)

main :: IO ()
main = do
  input <- readInput 2023 2
  let Right games = runParser (gameP `sepEndBy` newline) "" input
  print (part1 games)
  print (part2 games)
  --print . map cubePower . map smallestPossibleAim $ games
  --print (smallestPossibleAim (head games))

gameP :: Parser Game
gameP = do
  chunk "Game "
  gameId <- intP
  chunk ": "
  observations <- (MS.fromOccurList <$> observationP `sepBy` chunk ", ") `sepEndBy` chunk "; "
  pure (Game gameId observations)

observationP :: Parser (String, Int)
observationP = do
  count <- intP <* char ' '
  color <- many lowerChar
  pure (color, count)

isPossible :: MultiSet String -> Game -> Bool
isPossible aim (Game _ observations) = all (\s -> MS.isSubsetOf s aim) observations

smallestPossibleAim :: Game -> MultiSet String
smallestPossibleAim (Game _ observations) = foldl MS.maxUnion MS.empty observations

cubePower :: MultiSet String -> Int
cubePower = product . map snd . MS.toOccurList

part1 :: [Game] -> Int
part1 = sum . map gameId . filter (isPossible aim)
  where aim = MS.fromOccurList [("red", 12), ("green", 13), ("blue", 14)]

part2 :: [Game] -> Int
part2 = sum . map cubePower . map smallestPossibleAim