{-# LANGUAGE TemplateHaskell, TupleSections #-}
import Data.Foldable
import Data.Ord
import Control.Monad
import Data.Monoid
import Data.List
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Ix
import Data.Maybe
import Text.Parsec
import Control.Lens
import Data.Either
import Control.Monad.Writer.Lazy

type Parser a = Parsec String () a

data DateTime = Date
  { year   :: Int
  , month  :: Int
  , day    :: Int
  , hour   :: Int
  , minute :: Int
  } deriving (Eq, Ord)
instance Show DateTime where
  show (Date y m d h n) = concat
    [ padLeft 4 y, "-", padLeft 2 m, "-", padLeft 2 d, " "
    , padLeft 2 h, ":", padLeft 2 n ]
    where padLeft n x = replicate (n - length (show x)) '0' <> show x

data GuardShift = Guard
  { guardId :: Int
  , begins :: DateTime
  , naps :: [Nap]
  } deriving Show

type Nap = (DateTime, DateTime)

data GuardEvent = Begins DateTime Int | Wake DateTime | Sleep DateTime deriving Show
makePrisms ''GuardEvent

main :: IO ()
main = do
  file <- readFile "Day04.txt"
  --parseTest (parseGuardEvent `sepBy` newline)  file
  let Right events = sortOn eventTime <$> parse (parseGuardEvent `sepBy` newline) "" file
  let shifts = mkShifts events
  let sleepMap = M.fromListWith (+) (shiftSleeping <$> shifts) :: Map Int Int
  let sleepy = fst . last . sortOn (snd) $ M.assocs sleepMap :: Int
  let min = sleepiestMinute shifts sleepy
  -- print min
  print $ sleepy * (fst min)
  let allGuardIds = nub $ map guardId shifts
  print allGuardIds
  let x = maximumBy (comparing (snd . sleepiestMinute shifts)) allGuardIds
  print $ x * (fst $ sleepiestMinute shifts x)


  pure ()

--                                        (minute, INSTANCES)
sleepiestMinute :: [GuardShift] -> Int -> (Int, Int)
sleepiestMinute shifts gid =
  let guardNaps = filter ((gid==) . guardId) shifts >>= naps :: [Nap]
      napMap = M.fromListWith (+) $ guardNaps >>= napToMap :: Map Int Int
      in maximumBy (comparing snd) $ (-1, -1) : M.assocs napMap

napToMap :: Nap -> [(Int, Int)]
napToMap (d1, d2) = (,1) <$> [(minute d1) .. (minute d2 - 1)]

shiftSleeping :: GuardShift -> (Int, Int)
shiftSleeping (Guard n b naps) = (n, sum $ map minutes naps)

minutes :: (DateTime, DateTime) -> Int
minutes (Date _ _ _ _ m1, Date _ _ _ _ m2) = m2 - m1

eventTime :: GuardEvent -> DateTime
eventTime event = case event of
    Begins d _ -> d
    Wake d     -> d
    Sleep d    -> d

parseInt = read <$> (many1 digit)

parseGuardEvent :: Parser GuardEvent
parseGuardEvent = do
  date <- between (char '[') (char ']') parseDate
  space
  choice
    [ Begins date <$> (string "Guard #" *> parseInt <* string " begins shift")
    , Wake date <$ string "wakes up"
    , Sleep date <$ string "falls asleep"
    ]
  where
    parseDate :: Parser DateTime
    parseDate = Date
      <$> (read <$> count 4 digit) <* char '-'
      <*> (read <$> count 2 digit) <* char '-'
      <*> (read <$> count 2 digit) <* space
      <*> (read <$> count 2 digit) <* char ':'
      <*> (read <$> count 2 digit)

mkShifts :: [GuardEvent] -> [GuardShift]
mkShifts = execWriter . mkShift
  where
    mkShift :: [GuardEvent] -> Writer [GuardShift] [GuardEvent]
    mkShift [] = pure []
    mkShift (Begins d n:es) = do
      let (events, rest) = span (isn't _Begins) es
      tell [Guard n d (naps events)]
      mkShift rest
      where
        naps (Sleep d1 : Wake d2 : xs) = (d1, d2) : naps xs
        naps [] = []