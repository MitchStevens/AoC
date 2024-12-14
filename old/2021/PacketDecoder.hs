{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module PacketDecoder where

import Advent
import Binary
import Control.Lens
import Control.Monad
import Data.Bool
import Data.Char
import Data.Foldable
import Text.Megaparsec hiding (getInput)

data Packet = Packet {packetVersion :: Int, packetTypeId :: Int, packetType :: PacketType}
  deriving (Show)

data PacketType
  = Literal Int
  | Operator [Packet]
  deriving (Show)

--type Parser = ParsecT String Binary
--
--instance VisualStream Binary where
--  showTokens _ = show . Binary . toList
--
--instance TraversableStream Binary where
--  reachOffset n s = (Just (show $ pstateOffset s), s)
--
--instance ShowErrorComponent String where
--  showErrorComponent = id
--
--subversions :: Packet -> [Int]
--subversions (Packet version _ packetType) =
--  version : case packetType of
--    Literal _ -> []
--    Operator subpackets -> subpackets >>= subversions
--
--evalPacket :: Packet -> Int
--evalPacket = \case
--  Packet _ 0 (Operator packets) -> sum (map evalPacket packets)
--  Packet _ 1 (Operator packets) -> product (map evalPacket packets)
--  Packet _ 2 (Operator packets) -> minimum (map evalPacket packets)
--  Packet _ 3 (Operator packets) -> maximum (map evalPacket packets)
--  Packet _ 4 (Literal n) -> n
--  Packet _ 5 (Operator [p1, p2]) -> if evalPacket p1 > evalPacket p2 then 1 else 0
--  Packet _ 6 (Operator [p1, p2]) -> if evalPacket p1 < evalPacket p2 then 1 else 0
--  Packet _ 7 (Operator [p1, p2]) -> if evalPacket p1 == evalPacket p2 then 1 else 0
--
--binaryToInt :: Binary -> Int
--binaryToInt = sum . imap (\i a -> if a then 2 ^ i else 0) . reverse . getBinary
--
--binary :: Int -> Parser m Int
--binary n = undefined -- binaryToInt . Binary <$> count n anySingle
--
--packet :: Parser m Packet
--packet = do
--  packetVersion <- binary 3
--  packetTypeId <- binary 3
--  packetType <-
--    if packetTypeId == 4
--      then literal
--      else operator
--  pure (Packet packetVersion packetTypeId packetType)
--
--literal :: Parser m PacketType
--literal = Literal . hexToInt <$> literalRec
--  where
--    literalRec = do
--      b <- anySingle
--      cons <$> binary 4 <*> if b then literalRec else pure []
--
--operator :: Parser m PacketType
--operator =
--  Operator <$> do
--    lengthTypeId <- anySingle
--    case lengthTypeId of
--      False -> do
--        lengthOfSubpackets <- binary 15
--        initialOffset <- getOffset
--        let maxOffset = initialOffset + lengthOfSubpackets
--        let notFinished = (maxOffset >) <$> getOffset
--        many $ (notFinished >>= guard) *> packet
--      True -> do
--        n <- binary 11
--        count n packet
--
--day16 :: IO ()
--day16 = do
--  transmission <- head <$> readInput 16
--  print transmission
--  let hexString = "D2FE28"
--  let hex2 = "38006F45291200"
--  let hex3 = "EE00D40C823060"
--  print . hexToBinary $ hex3
--  let Right p = parse packet "" . hexToBinary $ transmission
--  print . sum $ subversions p
--  print $ evalPacket p
