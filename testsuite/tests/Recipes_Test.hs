{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-} -- Allow QuickCheck Arbitrary instances
{-# LANGUAGE OverloadedStrings #-}
module Recipes_Test where

import Test.Framework
import qualified Data.Text as T

import Recipes
import Data.Aeson

instance Arbitrary Quantity where
  arbitrary = Quantity <$> arbitrary <*> arbitrary <*> pure Nothing

instance Arbitrary Ingredient where
  arbitrary = do
    choice <- arbitrary
    if choice
      then Ingredient <$> fmap Left arbitrary <*> arbitrary
      else Ingredient <$> fmap Right arbitrary <*> arbitrary

-- | We need this type to avoid incredients that include many nested recipes.
newtype NonRecursiveIngredient = NonRecursiveIngredient { getIngredient :: Ingredient }

instance Arbitrary NonRecursiveIngredient where
  arbitrary = do
    name     <- arbitrary
    quantity <- arbitrary
    return (NonRecursiveIngredient (Ingredient (Left name) quantity))

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary Recipe where
  arbitrary = do
    name    <- arbitrary
    nonRecs <- listOf arbitrary
    instr   <- arbitrary
    return (Recipe name (map getIngredient nonRecs) instr Nothing)

prop_quantityToFrom :: Quantity -> Bool
prop_quantityToFrom q = case decode (encode q) of
  Just q' -> q == q'
  Nothing -> False

prop_ingredientToFrom :: Ingredient -> Bool
prop_ingredientToFrom q = case decode (encode q) of
  Just q' -> q == q'
  Nothing -> False

prop_recipeToFrom :: Recipe -> Bool
prop_recipeToFrom r = case decode (encode r) of
  Just r' -> r == r'
  Nothing -> False
