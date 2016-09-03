{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- Allow QuickCheck Arbitrary instances
{-# LANGUAGE OverloadedStrings #-}
module Recipes.Types_Test where

import Test.Framework
import qualified Data.Text as T
import Data.Aeson

import Recipes.Types

instance Arbitrary Recipe where
  arbitrary = do
    name    <- arbitrary
    nonRecs <- listOf arbitrary
    instr   <- arbitrary
    _id     <- arbitrary
    return (Recipe name (map getComponent nonRecs) instr _id)

instance Arbitrary Quantity where
  arbitrary = Quantity <$> arbitrary <*> arbitrary

instance Arbitrary Ingredient where
  arbitrary = Ingredient <$> arbitrary <*> arbitrary

instance Arbitrary Component where
  arbitrary = do
    choice <- arbitrary
    if choice
      then IngredientComponent <$> arbitrary <*> arbitrary <*> arbitrary
      else RecipeComponent <$> arbitrary <*> arbitrary <*> arbitrary

-- | We need this type to avoid Components that include many nested Recipes.
newtype NonRecursiveComponent = NonRecursiveComponent { getComponent :: Component }

instance Arbitrary NonRecursiveComponent where
  arbitrary = do
    ingredient <- arbitrary
    quantity   <- arbitrary
    _id        <- arbitrary
    return (NonRecursiveComponent (IngredientComponent quantity ingredient _id))

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

toFrom :: (Eq a, FromJSON a, ToJSON a) => a -> Bool
toFrom x = case decode (encode x) of
  Just x' -> x == x'
  Nothing -> False
  
prop_quantityToFrom :: Quantity -> Bool
prop_quantityToFrom = toFrom

prop_ingredientToFrom :: Ingredient -> Bool
prop_ingredientToFrom = toFrom

prop_recipeToFrom :: Recipe -> Bool
prop_recipeToFrom = toFrom

prop_componentToFrom :: Component -> Bool
prop_componentToFrom = toFrom
