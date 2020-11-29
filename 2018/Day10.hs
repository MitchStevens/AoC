{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}
import Text.Parsec (Parsec, char, optionMaybe, spaces, string, letter, sepBy, newline, parse, many1, digit, parseTest)
import Data.Char (ord)
import Data.List hiding (insert)
import Data.Monoid
import Data.Ord
import Data.Array
import Data.Traversable (for)
import Control.Monad.State.Strict
import Data.Foldable (traverse_, for_)
import Data.Bifunctor

import Data.Set (Set)
import qualified Data.Set as S
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Control.Lens

data Sky = Sky [Light]
instance Show Sky where
  show sky@(Sky lights) = 
    let positionSet = S.fromList $ _pos <$> lights
        posList = unzip $ _pos <$> lights
        ((minX, minY), (maxX, maxY)) = skyBounds sky
        line j = (\i -> if S.member (i, j) positionSet then '#' else '.') <$> [minX..maxX]
    in  intercalate "\n" $ line <$> [minY..maxY]

data Light = Light
  { _pos :: (Int, Int)
  , _vel :: (Int, Int) } deriving (Eq, Show)

main :: IO ()
main = do
  file <- readFile "Day10.txt"
  --parseTest (parseLight `sepBy` newline)  file
  let Right lights = parse (parseLight `sepBy` newline) "" file
  --let lights = [Light (0, 0) (1, 0), Light (0, 0) (-1, 0)]
  runSim (Sky lights) 0
  -- print (Sky lights)
  -- putStrLn ""
  -- print (nextSky $ Sky lights)



runSim :: Sky -> Int -> IO ()
runSim sky n = do
  when (suffientlyClustered sky) $ do
    putStrLn ("Day " <> (show n))
    print sky
  when (n < 100000) $
    runSim (nextSky sky) (n+1)

nextSky :: Sky -> Sky
nextSky (Sky lights) = Sky (nextLight <$> lights)
  where
    nextLight (Light (px, py) v@(vx, vy)) = Light (px + vx, py + vy) v

--                  min point   max point
skyBounds :: Sky -> ((Int, Int), (Int, Int))
skyBounds (Sky lights) =
  let posList = unzip $ _pos <$> lights
      minpoint = bimap minimum minimum posList
      maxpoint = bimap maximum maximum posList
  in (minpoint, maxpoint)

suffientlyClustered :: Sky -> Bool
suffientlyClustered sky = (maxX - minX) <= xBound && (maxY - minY) <= yBound
  where
    ((minX, minY), (maxX, maxY)) = skyBounds sky
    xBound = 150
    yBound = 150

parseLight :: Parsec String () Light
parseLight = do
  string "position="
  pos <- parsePair
  string " velocity="
  vel <- parsePair
  pure $ Light pos vel

parsePair :: Parsec String () (Int, Int)
parsePair = do
  char '<' *> spaces
  i <- parseInt
  char ',' *> spaces
  j <- parseInt
  char '>'
  pure (i, j)

parseInt :: Parsec String () Int
parseInt = do
  neg <- optionMaybe (char '-')
  ds <- many1 digit
  pure $ if neg == Nothing then read ds else - (read ds)