module Types where

import Options.Applicative
import Data.Char
import Data.List as L
import Data.Ix
import Text.Printf
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad

data AdventDate = AdventDate
  { year :: Int
  , day :: Int
  }
  deriving (Eq, Show)

data Language = Haskell
  deriving (Eq, Show, Enum, Bounded)

data Options = Options
  { adventDate :: AdventDate
  , lang :: Language
  }
  deriving (Show)

adventDateP :: Parser AdventDate
adventDateP = do
  year <- argument (parseNat (2015, maxBound)) -- advent will NEVER end
    ( metavar "YEAR"
    <> help "Advent problem year"
    )
  day <- argument (parseNat (1, 25)) 
    ( metavar "DAY"
    <> help "Advent problem day"
    )
  pure (AdventDate year day)
  where
    parseNat range = eitherReader $ \s -> do
      unless (all isDigit s) $
        Left (printf "%s is not a natural number" s)
      let nat = read s
      unless (inRange range nat) $
        Left (printf "%s is not in the range %s" s (show range))
      pure nat

languageP :: Parser Language
languageP = option (maybeReader readLang)
  ( long "language"
  <> short 'l'
  <> value Haskell
  <> help "Advent problem language (default Haskell) "
  )
  where
    readLang s = L.find (\lang -> map toLower (show lang) == map toLower s) [minBound .. maxBound]

optionsP :: Parser Options
optionsP = Options <$> adventDateP <*> languageP