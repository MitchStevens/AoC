module BinaryDiagnostic where

import Advent
import Binary
import Data.List 
import Data.Function

gammaRate :: [Binary] -> Binary
gammaRate = Binary . map mostCommon . transpose . map getBinary

epsilonRate :: [Binary] -> Binary
epsilonRate = Binary . map not . getBinary . gammaRate

day3 :: IO ()
day3 = do
  input <- map (read :: String -> Binary) <$> readInput 2021 3
  print (binaryToInt (gammaRate input) * binaryToInt (epsilonRate input))
