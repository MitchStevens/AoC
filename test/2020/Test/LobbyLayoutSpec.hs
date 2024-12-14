module Test.LobbyLayoutSpec where

import Test.Hspec
import Data.Set (Set)
import qualified Data.Set as S 
import LobbyLayout
import Control.Monad.State (evalState)
import Point (origin)

spec :: Spec
spec = describe "Lobby Layout" $ do
    describe "toggleTile" $ do
        it "" $ evalState (isBlack origin) S.empty `shouldBe` False