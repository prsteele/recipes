{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import qualified Data.Text.IO as T

import Recipes

showQuantity :: Quantity -> T.Text
showQuantity (Quantity name amount _) = T.intercalate " " [name, T.pack (show amount)]

showIngredient :: Ingredient -> T.Text
showIngredient (Ingredient (Left name) quantity) =
  T.intercalate " " [name, showQuantity quantity]
showIngredient (Ingredient (Right recipe) quantity) =
  T.intercalate " " [showRecipe recipe, showQuantity quantity]

showRecipe :: Recipe -> T.Text
showRecipe (Recipe name ingredients instructions _) =
  T.intercalate "\n" (name : (map showIngredient ingredients) ++ [instructions])

main :: IO ()
main = T.putStrLn . showRecipe $ Recipe "test" [Ingredient (Left "potato") (Quantity "count" 1 Nothing)] "some instructions" Nothing
