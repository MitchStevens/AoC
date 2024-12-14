module PassportProcessing where

import Data.List.Split
import Data.List
import Data.Char
import Data.Functor
import Control.Monad
import Advent (readInput)
import Data.Maybe (catMaybes, mapMaybe, fromMaybe)
import Data.Ix (Ix(inRange))
import Text.Read (readMaybe)
import Control.Applicative (Applicative(liftA2))

type Passport = [(String, String)]

validatePassport :: Passport -> Bool
validatePassport pp = fromMaybe False $
    foldl (liftA2 (&&)) (Just True)
        [ lookup "byr" pp >>= birthYear
        , lookup "iyr" pp >>= issueYear
        , lookup "eyr" pp >>= expirationYear
        , lookup "hgt" pp <&> height
        , lookup "hcl" pp <&> hairColor
        , lookup "ecl" pp <&> eyeColor
        , lookup "pid" pp <&> passportId
        ]
    where
        birthYear = fmap (inRange (1920, 2002)) . readMaybe
        issueYear = fmap (inRange (2010, 2020)) . readMaybe
        expirationYear = fmap (inRange (2020, 2030)) . readMaybe
        height h = 
            let (number, unit) = span isDigit h
            in case unit of
                "cm" -> inRange (150, 193) (read number)
                "in" -> inRange (59, 76) (read number)
                _ -> False
        hairColor ('#':hex) = all isHexDigit hex && length hex == 6
        hairColor _ = False
        eyeColor color = color `elem` (words "amb blu brn gry grn hzl oth")
        passportId ppid = (all isDigit ppid) && (length ppid == 9)

readPassport :: String -> Maybe Passport
readPassport str = do
    let pp = map pair (words str)
    lookup "byr" pp
    lookup "iyr" pp
    lookup "eyr" pp
    lookup "hgt" pp
    lookup "hcl" pp
    lookup "ecl" pp
    lookup "pid" pp
    pure pp
    where pair s = (takeWhile (':'/=) s, tail (dropWhile (':'/=) s))

testInput = 
    [ "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd"
    , "byr:1937 iyr:2017 cid:147 hgt:183cm"
    , ""
    , "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884"
    , "hcl:#cfa07d byr:1929"
    , ""
    , "hcl:#ae17e1 iyr:2013"
    , "eyr:2024"
    , "ecl:brn pid:760753108 byr:1931"
    , "hgt:179cm"
    , ""
    , "hcl:#cfa07d eyr:2025 pid:166559648"
    , "iyr:2011 ecl:brn hgt:59in"
    ]


day4 :: IO ()
day4 = do
    input <- readInput 2020 4
    let rawText = fmap unwords (splitOn [""] input)
    let unvalidatedPassports = mapMaybe readPassport rawText
    let validatedPassports = filter validatePassport unvalidatedPassports
    print (length unvalidatedPassports)
    print (length validatedPassports)