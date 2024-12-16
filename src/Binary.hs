module Binary where

import Data.Bool
import Data.Char (digitToInt, intToDigit, toUpper)
import Data.Bits
import Control.Monad
import Data.Word (Word8)
import Data.Function ((&))
import Control.Lens (FunctorWithIndex(imap))
import Data.List (unfoldr)

newtype Binary = Binary {getBinary :: [Bool]}
  deriving (Eq)

newtype Hex = Hex {getHex :: [Word8]}
  deriving (Eq)

instance Show Binary where
  show = map (bool '0' '1') . getBinary

instance Read Binary where
  readsPrec _ = maybe [] (\bs -> [(Binary bs, "")]) . traverse readBit
    where
      readBit :: Char -> Maybe Bool
      readBit '0' = Just False
      readBit '1' = Just True
      readBit _ = Nothing

instance Show Hex where
  show (Hex hex) = map (toUpper . intToDigit)  $ stripLeading 0 (hex >>= splitWord8)
    where
      splitWord8 :: Word8 -> [Int]
      splitWord8 n = [ fromIntegral (n `div` 16), fromIntegral (n `mod` 16) ]

instance Read Hex where
  readsPrec _ str = str
    & map (fromIntegral . digitToInt)
    & reverse
    & pair
    & reverse
    & map (\(b, a) -> maybe 0 (`shiftL` 4) a + b)
    & toHex
    & \hex -> [(hex , "")]
    where
      pair :: [a] -> [(a, Maybe a)]
      pair (a:b:cs) = (a, Just b) : pair cs
      pair [a]      = [(a, Nothing)]
      pair []       = []

stripLeading :: (Eq a) => a -> [a] -> [a]
stripLeading z (a:b:cs) = if a==z then stripLeading z (b:cs) else a:b:cs
stripLeading _ cs = cs

-- starts from the end of the list, hence all the list reverses
splitOnLength :: Int -> [a] -> [[a]]
splitOnLength n ls = ls
  & reverse
  & unfoldr (\l -> if not (null l) then Just (reverse (take n l), drop n l) else Nothing)
  & reverse

toBinary :: [Bool] -> Binary
toBinary [] = Binary [False]
toBinary bs =  Binary (stripLeading False bs)

toHex :: [Word8] -> Hex
toHex [] = Hex [0]
toHex ws = Hex (stripLeading 0 ws)

intToBinary :: Int -> Binary
intToBinary = toBinary . reverse . unfoldr (\b -> if b > 0 then Just (odd b, b `div` 2) else Nothing)

intToHex :: Int -> Hex
intToHex = toHex . reverse . unfoldr (\b -> if b > 0 then Just (fromIntegral (b `mod` 256), b `div` 256) else Nothing)

hexToBinary :: Hex -> Binary
hexToBinary (Hex hex) = toBinary (hex >>= asBinary)
  where asBinary n = [ testBit n b | b <- [7,6..0]]

binaryToHex :: Binary -> Hex
binaryToHex (Binary binary) = binary
  & splitOnLength 8
  & map (sum . imap (\i a -> if a then bit i else 0) . reverse)
  & toHex

binaryToInt :: Binary -> Int
binaryToInt = sum . imap (\i a -> if a then 2 ^ i else 0) . reverse . getBinary

hexToInt :: Hex -> Int
hexToInt = sum . imap (\i a -> fromIntegral a * 16 ^ i) . reverse . getHex

