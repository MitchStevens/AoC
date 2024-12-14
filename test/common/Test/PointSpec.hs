module Test.PointSpec where

import Test.Hspec
import Point
import Control.Monad (forM_)


main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Point" $ do
        describe "surrounding" $ do
            it "" $ length (surrounding origin) `shouldBe` 9
            it "" $ surrounding (0, 0) `shouldContain` [(0, 0)]
            it "" $ surrounding (6, 0) `shouldContain` [(7, 1)]
        describe "adjacent" $ do
            it "" $ length (adjacent (0, 0)) `shouldBe` 8
            it "" $ adjacent (0, 0) `shouldNotContain` [(0, 0)]
            it "" $ adjacent (6, 0) `shouldContain` [(7, 1)]
        describe "cardinal" $ do
            it "" $ length (cardinal (0, 0)) `shouldBe` 4
            it "" $ cardinal (6, 0) `shouldContain` [(6, 1)]
            it "" $ cardinal (6, 0) `shouldNotContain` [(7, 1)]
        describe "directions" $ do
            it "" $ up (1, 1) `shouldBe` (1, 0)
            it "" $ down (1, 1) `shouldBe` (1, 2)
            it "" $ left (1, 1) `shouldBe` (0, 1)
            it "" $ right (1, 1) `shouldBe` (2, 1)
