module DockingData where
import Data.Bits
import Advent
import Data.Word
import Data.Map (Map, (!))
import qualified Data.Map as M
import Control.Lens
import Text.Megaparsec
import Data.Void
import Text.Megaparsec.Char
import Data.Char (isDigit)
import Control.Monad.State
import Data.Either

type Parser = Parsec Void String

data Operation = BitMaskOp BitMask | WriteOp Write
    deriving Show

data BitMask = BitMask { bitMask :: Word64, value :: Word64 }
    deriving Show

data Write = Write { location :: Int, writeValue :: Word64 }
    deriving Show

type Memory = Map Int Word64

data S = S
    { _memory :: Memory
    , _bitmask :: BitMask
    } deriving Show
makeLenses ''S

operationP :: Parser Operation
operationP = choice . map try $ [BitMaskOp <$> bitMaskP, WriteOp <$> writeP]

bitMaskP :: Parser BitMask
bitMaskP = do
    string "mask = "
    bits <- takeP Nothing 36
    let bitMask = ifoldr (\i a b -> if a == 'X' then b else setBit b (35-i)) zeroBits bits
    let value = ifoldr (\i a b -> if a == '1' then setBit b (35-i) else b) zeroBits bits
    pure (BitMask bitMask value)

writeP :: Parser Write
writeP = do
    string "mem["
    location <- intP
    string "] = "
    writeValue <- fromInteger . toInteger  <$> intP
    pure (Write location writeValue)
    where
        intP = (read :: String -> Int) <$> takeWhile1P Nothing isDigit

mask :: BitMask -> Word64 -> Word64
mask (BitMask bitmask value) word = (word .&. complement bitmask) .|. value

decoder1 :: (MonadState S m) => Operation -> m ()
decoder1 = \case
    BitMaskOp bm -> do
        bitmask .= bm
    WriteOp (Write location writeValue) -> do
        bm <- gets _bitmask
        memory %= M.insertWith (.|.) location zeroBits
        memory . ix location .= writeValue
        memory . ix location %= mask bm

test1 = 
    [ "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
    , "mem[8] = 11"
    , "mem[7] = 101"
    , "mem[8] = 0"
    ]

day14 :: IO ()
day14 = do
    --let input = test1
    input <- readInput 2020 14
    let operations = rights . map (parse operationP "") $ input
    --print $ zeroBits
    --    & mask (BitMask (bit 1 .|. bit 6) (bit 6))
    --print operations

    print . sum . _memory $ flip execState (S M.empty (BitMask zeroBits zeroBits)) $ do
        (traverse decoder1 operations)







