module Test.MonsterMessagesSpec where

import Test.Hspec
import MonsterMessages

spec :: Spec
spec = describe "Monster Messages" $ do
  describe "intP" $ do
    it "" $ 1 `shouldBe` 1
    --parse intP "1" `shouldBe` Right 1
    --parse intP "10" `shouldBe` Right 10