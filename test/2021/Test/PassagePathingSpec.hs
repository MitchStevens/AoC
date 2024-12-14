module Test.PassagePathingSpec where

import PassagePathing
import Test.Hspec
import PassagePathing (appendPath, makeCaveSystem)
import Data.Set (fromList)
import qualified Data.Map as M
import qualified Data.Set as S

caves :: CaveSystem
caves = makeCaveSystem
    [(Start, Big "A")
    , (Start, Small "b") 
    , (Big "A", Small "c") 
    , (Big "A", Small "b") 
    , (Small "b", Small "d") 
    , (Big "A", End) 
    , (Small "b", End) 
    ]

spec :: Spec
spec = describe "Passage Pathing" $ do
    --describe "makeCaveSystem" $ do
    --    it "" $ makeCaveSystem [(Start, End)] `shouldBe` M.singleton Start [End]
    describe "appendPath" $ do
        it "" $ appendPath Start [] `shouldBe` Just [Start]
        it "" $ appendPath Start [End] `shouldBe` Just [Start, End]
        it "" $ appendPath (Big "A") [Small "b"] `shouldBe` Just [Big "A", Small "b"]
        it "" $ appendPath (Small "a") [Small "b"] `shouldBe` Just [Small "a", Small "b"]
        it "" $ appendPath (Small "b") [Small "b"] `shouldBe` Nothing
        it "" $ appendPath Start [Start] `shouldBe` Nothing
        it "" $ appendPath End [End] `shouldBe` Nothing
    --describe "paths" $ do
    --    it "" $ paths caves (Small "b") End `shouldBe` fromList [[Small "b", End]]


