import Text.Parsec
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type Parser a = Parsec String () a

data TuringState = TuringState
  { ifFalse :: [TuringOperation]
  , ifTrue  :: [TuringOperation]
  }  deriving Show

data TuringOperation
  = Write Bool
  | Move (Either () ())
  | Continue Char deriving Show

data TuringMachine = TuringMachine
  { tape :: TuringTape
  , states :: Map Char TuringState
  , currentState :: Char
  } deriving Show


data TuringTape = Tape
  { left :: [Bool]
  , value :: Bool
  , right :: [Bool]
  } deriving Show

main :: IO ()
main = do
  file <- readFile "Day25.txt"
  parseTest parseTuring file

write :: Bool -> TuringTape -> TuringTape
write b (Tape l _ r) = Tape l b r

moveLeft :: TuringTape -> TuringTape
moveLeft (Tape (l:ls) v r) = Tape ls l (v:r)
moveLeft (Tape [] v r) = Tape [] False (v:r)

moveRight :: TuringTape -> TuringTape
moveRight (Tape l v (r:rs)) = Tape (v:l) r rs
moveRight (Tape l v []) = Tape (v:l) False []

parseTuring :: Parser (TuringMachine, Int)
parseTuring = do
  currentState <- string "Begin in state " *> letter <* string ".\n"
  n <- string "Perform a diagnostic checksum after " *> (many digit) <* string " steps.\n\n"
  states <- parseState `sepBy` newline
  pure $
    ( TuringMachine (Tape [] False []) (M.fromList states) currentState
    , read n)

parseState :: Parser (Char, TuringState)
parseState = do
  name <- string "In state " *> letter <* string ":\n"
  string "  If the current value is 0:\n"
  ifFalse <- (try parseOperation) `sepBy` newline
  string "  If the current value is 1:\n"
  ifTrue <- parseOperation `sepBy` newline
  pure (name, TuringState ifFalse ifTrue)

  where
    parseOperation = string "    - " *> choice
      [ string "Write the value " *> (Write <$> b)
      , string "Move one slot to the " *> (Move <$> l)
      , string "Continue with state " *> (Continue <$> letter)] <* char '.'

    b = choice [ False <$ char '0', True <$ char '1']
    l = choice [ Left () <$ string "left", Right () <$ string "right"]