-- |

module Day14 where

import Data.Bits
import Control.Monad.State.Strict
import Control.Monad.Trans.State.Strict
import Data.IntMap.Strict as M
import Control.Monad

data BitMask = BitMask { zeroes :: Int, ones :: Int } -- zeros contains all ones, except for where there should be a zero
data Operation = SetBitMask BitMask | SetAddress Int Value



readBinary :: [Int] -> Int
readBinary =
  polyEval . reverse
  where

parseMask :: String -> Operation
parseMask str =
  let
    polyEval [] = 0
    polyEval (x:xs) = x + 2 * polyEval xs

    mask = drop 7 str
    zeroes = polyEval . reverse . map (\c -> if c=='X' then 0 else digitToInt c) $ mask
    ones   = polyEval . reverse . map (\c -> if c=='X' then 1 else digitToInt c) $ mask
  in
    SetBitMask (BitMask zeroes ones)

parseMem :: String -> Operation
parseMem str =
  let
    (location, rest) = span (/=']')$ drop 4 str
    value = drop 4 rest
  in
    SetAddress (read location) (read value)

parseOperation :: String -> Operation
parseOperation str = case take 3 str of
  "mas" -> parseMask str
  "mem" -> parseMem str

main :: IO ()
main = do
  operations <- map parseOperation . lines <$> readFile "Day14Input.txt"
  traverse runOperation operations

mask :: BitMask -> Int -> Int
mask (BitMask zeros ones) = (ones .|.) . (zeros .&.)

runOperation :: Operation -> State (BitMask, IntMap Int) ()
runOperation (bitmask, map) = \case
  SetBitMask newMask a = put newMask
  SetAddress n val = do
    maskedVal <- gets fst <$> val
    modify (M.insert k maskedVal)
