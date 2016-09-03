{-# OPTIONS_GHC -fno-warn-orphans #-} -- Allow QuickCheck Arbitrary instances
module Recipes.Arbitrary where

import qualified Data.Text as T
import Test.Framework

import Recipes.Types

instance Arbitrary Recipe where
  arbitrary = do
    name    <- arbitrary
    nonRecs <- listOf arbitrary
    instr   <- arbitrary
    return (Recipe name (map getComponent nonRecs) instr Nothing)

instance Arbitrary Quantity where
  arbitrary = Quantity <$> arbitrary <*> arbitrary

instance Arbitrary Ingredient where
  arbitrary = Ingredient <$> arbitrary <*> pure Nothing

instance Arbitrary Component where
  arbitrary = do
    choice <- arbitrary
    if choice
      then IngredientComponent <$> arbitrary <*> arbitrary <*> pure Nothing
      else RecipeComponent <$> arbitrary <*> arbitrary <*> pure Nothing

-- | We need this type to avoid Components that include many nested Recipes.
newtype NonRecursiveComponent = NonRecursiveComponent { getComponent :: Component }

instance Arbitrary NonRecursiveComponent where
  arbitrary = do
    ingredient <- arbitrary
    quantity   <- arbitrary
    _id        <- arbitrary
    return (NonRecursiveComponent (IngredientComponent quantity ingredient _id))

instance Arbitrary T.Text where
  arbitrary = T.pack . filter (/= '\0') <$> arbitrary

