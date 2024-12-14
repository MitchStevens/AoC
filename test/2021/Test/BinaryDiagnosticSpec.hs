
module Test.BinaryDiagnosticSpec where

import Test.Hspec
import Binary
import BinaryDiagnostic

testInput :: [Binary]
testInput = map (read :: String -> Binary) $
    [ "00100" , "11110" , "10110" , "10111"
    , "10101" , "01111" , "00111" , "11100"
    , "10000" , "11001" , "00010" , "01010" ]

spec :: Spec
spec = describe "Binary Diagnostic" $ do
  describe "gamma rate" $ do
    it "" $ gammaRate testInput `shouldBe` read "10110"
  describe "epsilon rate" $ do
    it "" $ epsilonRate testInput `shouldBe` read "01001"