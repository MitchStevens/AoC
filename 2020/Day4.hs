{-# LANGAUGE OverloadedStrings #-}
module Day4 where

import Data.Attoparsec.Text
import Data.Text (Text, pack)
import Data.Text.IO as Text
import Data.Foldable
import Data.Maybe

newtype Passport = Passport [(String, String)] deriving Show

parseKeyValue :: Parser (String, String)
parseKeyValue = do
  key   <- many' (satisfy (`notElem` ":")) <* char ':'
  value <- many' (satisfy (`notElem` " \n"))
  pure (key, value)

parsePassport :: Parser Passport
parsePassport = Passport <$> sepBy parseKeyValue (satisfy (`elem`" \n"))

parseAll :: Parser [Passport]
parseAll = sepBy parsePassport (string $ pack "\n\n")

hasField :: Passport -> String -> Bool
hasField (Passport kvs) key = isJust (lookup key kvs)

validPassport :: Passport -> Bool
validPassport passport = all (hasField passport) fields
  where fields = ["byr","iyr","eyr","hgt","hcl","ecl","pid","cid"]


a = pack "hcl:#6b5442 ecl:brn iyr:2019\npid:637485594 hgt:171cm\neyr:2021 byr:1986"

main :: IO ()
main = do
  file <- Text.readFile "Day4Input.txt"
  --print file
  parseTest parsePassport a
  parseTest parseAll file
  --let Right passports = runParser parseAll () undefined file
  --traverse_ print passports
  --print . length $ passports
