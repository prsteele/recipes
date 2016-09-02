{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Types where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T

-- | A Recipe consisting of a name, a number of components, and
-- instructions.
data Recipe = Recipe
              { _recipeName         :: T.Text
              , _recipeComponents   :: [Component]
              , _recipeInstructions :: T.Text
              , _recipeId           :: Maybe Integer
              }
            deriving (Show, Ord, Eq)

instance FromJSON Recipe where
  parseJSON (Object v) = Recipe <$> name <*> components <*> instructions <*> _id
    where
      name         = v .:  "name"
      components   = v .:  "components"
      instructions = v .:  "instructions"
      _id          = v .:? "id"
  parseJSON x = typeMismatch "recipe" x

instance ToJSON Recipe where
  toJSON (Recipe name components instructions (Just _id)) =
    object [ "name"         .= name
           , "components"   .= components
           , "instructions" .= instructions
           , "id"           .= _id
           ]
  toJSON (Recipe name components instructions Nothing) =
    object [ "name"         .= name
           , "components"   .= components
           , "instructions" .= instructions
           ]

-- | An amount and an associated unit.
data Quantity = Quantity
                { _quantityName   :: T.Text
                , _quantityAmount :: Double
                }
              deriving (Show, Ord, Eq)

instance FromJSON Quantity where
  parseJSON (Object v) = Quantity <$> name <*> amount
    where
      name   = v .:  "name"
      amount = v .:  "amount"
  parseJSON x = typeMismatch "quantity" x

instance ToJSON Quantity where
  toJSON (Quantity name amount) = object [ "name"   .= name
                                         , "amount" .= amount
                                         ]

-- | The name of a basic ingredient.
data Ingredient = Ingredient
                  { _ingredientName :: T.Text
                  , _ingredientId   :: Maybe Integer
                  }
                deriving (Show, Ord, Eq)

instance FromJSON Ingredient where
  parseJSON (Object v) = Ingredient <$> name <*> _id
    where
      name = v .:  "name"
      _id  = v .:? "id"
  parseJSON x = typeMismatch "ingredient" x

instance ToJSON Ingredient where
  toJSON (Ingredient name (Just _id)) = object [ "name" .= name
                                               , "id"   .= _id
                                               ]
  toJSON (Ingredient name Nothing)    = object [ "name" .= name
                                               ]

-- | Either an Ingredient or a Recipe, along with a quantity.
data Component = IngredientComponent
                 { _componentQuantity   :: Quantity
                 , _componentIngredient :: Ingredient
                 , _componentId         :: Maybe Integer
                 }
               | RecipeComponent
                 { _componentQuantity :: Quantity
                 , _componentRecipe   :: Recipe
                 , _componentId       :: Maybe Integer
                 }
               deriving (Show, Ord, Eq)

instance FromJSON Component where
  parseJSON (Object v) = do
      quantity    <- v .:  "quantity"
      mrecipe     <- v .:? "recipe"
      mingredient <- v .:? "ingredient"
      _id         <- v .:? "id"
      let msg  = "Must specify one of \"ingredient\" or \"recipe\""
      case (mrecipe, mingredient) of
        (Just recipe, Nothing) ->
          return $ RecipeComponent quantity recipe _id
        (Nothing, Just ingredient) ->
          return $ IngredientComponent quantity ingredient _id
        _ ->
          fail msg
  parseJSON x = typeMismatch "component" x

instance ToJSON Component where
  toJSON (IngredientComponent quantity ingredient (Just _id)) =
    object [ "ingredient" .= ingredient
           , "quantity"   .= quantity
           , "id"         .= _id
           ]
  toJSON (IngredientComponent quantity ingredient Nothing) =
    object [ "ingredient" .= ingredient
           , "quantity"   .= quantity
           ]
  toJSON (RecipeComponent quantity recipe (Just _id)) =
    object [ "recipe"   .= recipe
           , "quantity" .= quantity
           , "id"       .= _id
           ]
  toJSON (RecipeComponent quantity recipe Nothing) =
    object [ "recipe"   .= recipe
           , "quantity" .= quantity
           ]

makeLenses ''Quantity
makeLenses ''Ingredient
makeLenses ''Recipe
makeLenses ''Component
