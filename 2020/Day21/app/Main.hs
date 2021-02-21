module Main where

import Text.Parsec
import Text.Parsec.Char
import Data.SBV
import Data.Functor
import Data.SBV.Trans.Control
import Control.Monad
import Data.Map (Map, (!))
import qualified Data.Map as M


newtype Ingredient = Ingredient String
  deriving (Eq, Ord, Show)
newtype Allergen = Allergen String
  deriving (Eq, Ord, Show)
data Recipe = Recipe { ingredients :: [Ingredient]
                     , allergens :: [Allergen] }
  deriving (Eq, Ord, Show)

data Fact = Contains Ingredient Allergen deriving (Eq, Ord, Show)
type FactBook = Map Fact SBool

recipeP :: Parsec String () Recipe
recipeP = do
  ingredients <- endBy ingredientP (char ' ')
  allergens <- between (char '(') (char ')') $ do
    string "contains "
    sepBy allergenP (string ", ")
  pure (Recipe ingredients allergens)
  where
    ingredientP = Ingredient <$> many1 lower
    allergenP = Allergen <$> many1 lower

main :: IO ()
main = do
  recipeStrings <- lines <$> readFile "test_input.txt"
  let Right recipes = traverse (parse recipeP "") recipeStrings
  let allAllergens = recipes >>= allergens
  let allIngredients = recipes >>= ingredients
  let allFacts = recipes >>= facts

  let a = (do
             factBook <- createVariables allFacts
             pure $ sAnd $ map (recipeConstraints factBook) recipes
          )
  aa <- sat a
  print aa
  print "end"

facts :: Recipe -> [Fact]
facts (Recipe ingredients allergens) =
  Contains <$> ingredients <*> allergens

createVariables :: [Fact] -> Symbolic (Map Fact SBool)
createVariables facts = fmap M.fromList . forM facts $ \fact -> do
  var <- sBool (show fact)
  pure (fact, var)

noAllergens :: FactBook
            -> Ingredient
            -> SBool
noAllergens vars ingredient = sAnd factVars
  where factVars = M.elems $ M.filterWithKey (\(Contains i _) _ -> i == ingredient) vars

recipeConstraints :: FactBook -> Recipe -> Symbolic SBool
recipeConstraints vars (Recipe ingredients allergens) =
  sAnd $ allergens <&> \allergen ->
    sOr $ ingredients <&> \ingredient ->
      vars ! (ingredient `Contains` allergen)
