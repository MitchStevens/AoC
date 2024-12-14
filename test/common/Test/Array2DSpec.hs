module Test.Array2DSpec where

import Test.Hspec
import Point
import Control.Monad (forM_)
import Array2D
import Data.Char (digitToInt)
import Data.Array (Array)


main :: IO ()
main = hspec spec

{-
  1 2 3
  4 5 6
  7 8 9
-}
array2D :: Array Point Int
array2D = readArray2D digitToInt ["123", "456", "789"]

spec :: Spec
spec = do
  describe "Array2D" $ do
    describe "look" $ do
      it "" $ look (1, 1) up array2D `shouldBe` [5, 2]
      it "" $ look (1, 1) down array2D `shouldBe` [5, 8]
      it "" $ look (1, 1) left array2D `shouldBe` [5, 4]
      it "" $ look (1, 1) right array2D `shouldBe` [5, 6]
      it "" $ look (0, 0) right array2D `shouldBe` [1, 2, 3]
      it "" $ look (0, 0) down array2D `shouldBe` [1, 4, 7]
      it "" $ look (0, 0) (right.down) array2D `shouldBe` [1, 5, 9]
