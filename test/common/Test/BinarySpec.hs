module Test.BinarySpec where

import Test.Hspec 
import Binary
import Control.Monad (forM_)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Binary" $ do
    describe "stripLeading" $ do
      it "" $ stripLeading '0' "0" `shouldBe` "0"
      it "" $ stripLeading '0' "00" `shouldBe` "0"
      it "" $ stripLeading '0' "01" `shouldBe` "1"
    describe "splitOnLength" $ do
      it "" $ splitOnLength 1 "" `shouldBe` []
      it "" $ splitOnLength 2 "" `shouldBe` []
      it "" $ splitOnLength 1 "a" `shouldBe` ["a"]
      it "" $ splitOnLength 2 "ab" `shouldBe` ["ab"]
      it "" $ splitOnLength 2 "abc" `shouldBe` ["a", "bc"]
    describe "read :: String -> Binary" $ do
      it "" $ read "0"  `shouldBe` toBinary [False]
      it "" $ read "1"  `shouldBe` toBinary [True]
      it "" $ read "10" `shouldBe` toBinary [True, False]
      it "" $ read "10" `shouldBe` toBinary [True, False]
    describe "read :: String -> Hex" $ do
      it "" $ read "0"   `shouldBe` toHex [0]
      it "" $ read "1"   `shouldBe` toHex [1]
      it "" $ read "9"   `shouldBe` toHex [9]
      it "" $ read "A"   `shouldBe` toHex [10]
      it "" $ read "F"   `shouldBe` toHex [15]
      it "" $ read "10"  `shouldBe` toHex [16]
      it "" $ read "FF"  `shouldBe` toHex [255]
      it "" $ read "100" `shouldBe` toHex [1, 0]
    describe "show :: Binary -> String" $ do
      it "" $ show (toBinary [False]) `shouldBe` "0"
      it "" $ show (toBinary [True]) `shouldBe` "1"
      it "" $ show (toBinary [True, False]) `shouldBe` "10"
      it "" $ show (toBinary [True, True]) `shouldBe` "11"
    describe "show :: Hex -> String" $ do
      it "" $ show (toHex [0]) `shouldBe` "0"
      it "" $ show (toHex [1]) `shouldBe` "1"
      it "" $ show (toHex [9]) `shouldBe` "9"
      it "" $ show (toHex [10]) `shouldBe` "A"
      it "" $ show (toHex [15]) `shouldBe` "F"
      it "" $ show (toHex [16]) `shouldBe` "10"
      it "" $ show (toHex [255]) `shouldBe` "FF"
      it "" $ show (toHex [1, 0]) `shouldBe` "100"
    describe "intToBinary" $ do
      it "" $ intToBinary 0 `shouldBe` toBinary [False]
      it "" $ intToBinary 1 `shouldBe` toBinary [True]
      it "" $ intToBinary 2 `shouldBe` toBinary [True, False]
      it "" $ intToBinary 3 `shouldBe` toBinary [True, True]
      it "" $ intToBinary 4 `shouldBe` toBinary [True, False, False]
    describe "intToHex" $ do
      it "" $ intToHex 0   `shouldBe` toHex [0]
      it "" $ intToHex 1   `shouldBe` toHex [1]
      it "" $ intToHex 15  `shouldBe` toHex [15]
      it "" $ intToHex 16  `shouldBe` toHex [16]
      it "" $ intToHex 255 `shouldBe` toHex [255]
      it "" $ intToHex 256 `shouldBe` toHex [1, 0]
    describe "hexToBinary" $ forM_ [0, 1, 8, 9, 15, 16, 255, 256] $ \n ->
      it "" $ hexToBinary (intToHex n) `shouldBe` intToBinary n
    describe "binaryToHex" $ forM_ [0, 1, 8, 9, 15, 16, 255, 256] $ \n ->
      it "" $ binaryToHex (intToBinary n) `shouldBe` intToHex n