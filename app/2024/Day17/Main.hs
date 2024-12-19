import Advent
import Parsers
import Data.Char
import Data.Bits as Bits
import Control.Lens
import Control.Monad.State.Class
import Data.Foldable
import Control.Monad.Writer.Class
import Control.Monad.RWS.Lazy
import Data.List
import Numeric
import Data.Set (Set)
import qualified Data.Set as S

data Register = A | B | C

data ComboOperand = Lit Integer | Reg Register

data Instruction
  = Adv ComboOperand
  | Bxl Int
  | Bst ComboOperand
  | Jnz Int
  | Bxc
  | Out ComboOperand
  | Bvd ComboOperand
  | Cvd ComboOperand

data Computer = Computer
  { _regA :: Integer
  , _regB :: Integer
  , _regC :: Integer
  , _program :: [Int]
  , _instPointer :: Int
  }
  deriving (Eq, Show)

makeLenses ''Computer

lookupReg :: (MonadState Computer m) => Register -> m Integer
lookupReg = \case
  A -> use regA
  B -> use regB
  C -> use regC

lookupCombo :: (MonadState Computer m) => ComboOperand -> m Integer
lookupCombo = \case
  Lit n -> pure n
  Reg reg -> lookupReg reg

comboOperand :: Int -> Maybe ComboOperand
comboOperand = \case
  0 -> Just (Lit 0)
  1 -> Just (Lit 1)
  2 -> Just (Lit 2)
  3 -> Just (Lit 3)
  4 -> Just (Reg A)
  5 -> Just (Reg B)
  6 -> Just (Reg C)
  7 -> Nothing
  _ -> Nothing

instruction :: Int -> Int -> Maybe Instruction
instruction code operand = case code of
  0 -> Adv <$> combo
  1 -> Just (Bxl operand)
  2 -> Bst <$> combo
  3 -> Just (Jnz operand)
  4 -> Just Bxc
  5 -> Out <$> combo
  6 -> Bvd <$> combo
  7 -> Cvd <$> combo
  _ -> Nothing
  where
    combo = comboOperand operand

lookupInstruction :: (MonadState Computer m) => m (Maybe Instruction)
lookupInstruction = do
  i <- use instPointer
  prog <- use program

  case traverse (prog !?) [i, i+1] of
    Just [ code, prog ] -> pure (instruction code prog)
    _ -> pure Nothing


runInstruction :: (MonadState Computer m, MonadWriter [Int] m) => Instruction -> m ()
runInstruction = \case
  Adv combo -> do
    n <- lookupCombo combo
    regA %= (`div` 2 ^ n)
    instPointer += 2
  Bxl n -> do
    regB %= (`xor` (toInteger n))
    instPointer += 2
  Bst combo -> do
    n <- lookupCombo combo
    regB .= (n `mod` 8)
    instPointer += 2
  Jnz n -> do
    a <- use regA
    if a == 0
      then instPointer += 2
      else instPointer .= n
  Bxc -> do
    c' <- use regC
    regB %= (`xor` c')
    instPointer += 2
  Out combo -> do
    n <- lookupCombo combo
    tell [ fromIntegral n `mod` 8 ]
    instPointer += 2
  Bvd combo -> do
    n <- lookupCombo combo
    a <- use regA
    regB .= (a `div` 2 ^ n)
    instPointer += 2
  Cvd combo -> do
    n <- lookupCombo combo
    a <- use regA
    regC .= (a `div` 2 ^ n)
    instPointer += 2

runProgram :: (MonadState Computer m, MonadWriter [Int] m) => m ()
runProgram = do
  instruction <- lookupInstruction
  case instruction of
    Just inst -> runInstruction inst *> runProgram
    Nothing -> pure ()

execute :: Computer -> [Int]
execute = snd . execRWS (runProgram :: RWS () [Int] Computer ()) ()

testInput = unlines
  [ "Register A: 729"
  , "Register B: 0"
  , "Register C: 0"
  , ""
  , "Program: 0,1,5,4,3,0"
  ]

testQuine = Computer 117440 0 0 [0,3,5,4,3,0] 0

main :: IO ()
main = do
  input <- readInput 2024 17
  let Right computer = runParser computerP "" input
  putStrLn (part1 computer)
  print (part2 computer)

regP :: String -> Parser Int
regP reg = chunk ("Register " <> reg <> ": ") *> intP

computerP :: Parser Computer
computerP = do
  regA <- toInteger <$> regP "A" <* newline
  regB <- toInteger <$> regP "B" <* newline
  regC <- toInteger <$> regP "C" <* newline
  newline
  chunk "Program: "
  program <- intP `sepEndBy` char ','
  pure (Computer regA regB regC program 0)

part1 = intercalate "," . map show . execute

part2 (Computer _ regB regC program instPointer) = minimum $
  foldl (\b n -> b `setBind` improveQuineish n) (S.singleton 0) [0..(length program)]
  
  where
    
    showBinary n = showIntAtBase 8 intToDigit n ""

    computer regA = Computer regA regB regC program instPointer

    isQuineish :: Int -> Integer -> Bool
    isQuineish n regA = take n (_program (computer regA)) == take n (execute (computer regA))

    improveQuineish :: Int -> Integer -> S.Set Integer
    improveQuineish n quineish = S.filter (isQuineish (n+1)) $ potentialImprovements
      where
        potentialImprovements = S.fromList [ i * (8^n) + quineish | i <- [0..512]]

    setBind :: Ord b => Set a -> (a -> Set b) -> Set b
    setBind set f = S.unions (S.map f set)

{-


Program: 
2,4 - B = last digit of A
1,2 - B = B xor 2
7,5 - C = A `div (2^B)
4,1 - B = B xor C
1,3 - B = B xor 3
5,5 - out (last digit of B)
0,3 - A div 8
3,0 - goto 0 if a /=0

C = A `div (2^((last digit of a) xor 2))
out (last a xor last C xor 1))
A div 8
goto 0 if a /=0



-}
