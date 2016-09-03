{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Recipes.Types_Test where

import Test.Framework
import Data.Aeson

import Recipes.Types
import Recipes.Arbitrary ()

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
