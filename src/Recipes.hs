{-# LANGUAGE OverloadedStrings #-}
module Recipes where

import Data.Aeson
import Data.Aeson.Types
import Data.Either
import GHC.Generics
import qualified Data.Text as T

-- | An amount and a unit name.
data Quantity = Quantity
                { _quantityName :: T.Text
                , _quantityAmount :: Double
                }
              deriving (Show, Ord, Eq)

instance FromJSON Quantity where
  parseJSON (Object v) = Quantity <$> v .: "name" <*> v .: "amount"
  parseJSON x          = typeMismatch "quantity" x

instance ToJSON Quantity where
  toJSON (Quantity name amount) = object [ "name" .= name
                                         , "amount" .= amount
                                         ]

-- | Either the name of an ingredient or a recipe, along with a
-- quantity
data Ingredient = Ingredient
                  { _ingredient :: Either T.Text Recipe
                  , _ingredientQuantity :: Quantity
                  }
                deriving (Show, Ord, Eq)

instance FromJSON Ingredient where
  parseJSON (Object v) = do
    ingredient <- v .:! "ingredient"
    recipe     <- v .:! "recipe"
    quantity   <- v .:  "quantity"
    case (ingredient, recipe) of
        (Just _, Just _)  -> fail "Cannot specify an ingredient and a recipe"
        (Just x, Nothing) -> return $ Ingredient (Left x)  quantity
        (Nothing, Just x) -> return $ Ingredient (Right x) quantity
        otherwise         -> fail "Must specify either \"ingredient\" or \"recipe\""
  parseJSON x = typeMismatch "ingredient" x

-- | A list of ingredients, along with text instructions.
data Recipe = Recipe
              { _recipeName :: T.Text
              , _recipeIngredients :: [Ingredient]
              , _recipeInstructions :: T.Text
              }
            deriving (Show, Ord, Eq)

instance FromJSON Recipe where
  parseJSON (Object v) = Recipe <$> v .: "name" <*> v .: "ingredients" <*> v .: "instructions"
