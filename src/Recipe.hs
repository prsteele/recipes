{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Recipe where

import Control.Lens hiding ((.=))
import qualified Data.Text as T

-- | An amount and an associated unit.
data Quantity = Quantity
                { _quantityName   :: T.Text
                , _quantityAmount :: Double
                }
              deriving (Show, Ord, Eq)


data Ingredient = Ingredient
                  { _ingredientName :: T.Text
                  , _ingredientId   :: Maybe Integer
                  }
                deriving (Show, Ord, Eq)


data Recipe = Recipe
              { _recipeName       :: T.Text
              , _recipeComponents :: [Component]
              , _recipeId         :: Maybe Integer
              }
            deriving (Show, Ord, Eq)


data Component = IngredientComponent
                 { _componentQuantity   :: Double
                 , _componentUnit       :: T.Text
                 , _componentIngredient :: Ingredient
                 }
               | RecipeComponent
                 { _componentQuantity :: Double
                 , _componentUnit     :: T.Text
                 , _componentRecipe   :: Ingredient
                 }
               deriving (Show, Ord, Eq)

makeLenses ''Quantity
makeLenses ''Ingredient
makeLenses ''Recipe
makeLenses ''Component
