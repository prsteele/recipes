{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- | We provide basic data types to represent recipes that can be
-- serialized to and deserialized from JSON.
module Recipes where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Types
-- import Database.PostgreSQL.Simple
-- import Database.PostgreSQL.Simple.ToField
-- import Database.PostgreSQL.Simple.ToRow
-- import Database.PostgreSQL.Simple.FromRow
import qualified Data.Text as T

-- | An amount and a unit name.
data Quantity = Quantity
                { _quantityName   :: T.Text
                , _quantityAmount :: Double
                , _quantityId     :: Maybe Int
                }
              deriving (Show, Ord, Eq)
makeLenses ''Quantity

instance FromJSON Quantity where
  parseJSON (Object v) = Quantity <$> name <*> amount <*> _id
    where
      name   = v .:  "name"
      amount = v .:  "amount"
      _id    = v .:? "id"
  parseJSON x          = typeMismatch "quantity" x

instance ToJSON Quantity where
  toJSON (Quantity name amount _id) = object [ "name"   .= name
                                             , "amount" .= amount
                                             , "id"     .= _id
                                             ]

-- instance ToRow Quantity where
--   toRow (Quantity name amount (Just _id)) = [ toField name
--                                             , toField amount
--                                             , toField _id
--                                             ]
--   toRow (Quantity name amount Nothing)    = [ toField name
--                                             , toField amount
--                                             ]

-- instance FromRow Quantity where
--   fromRow = Quantity <$> field <*> field <*> field

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
        _                 -> fail "Must specify either \"ingredient\" or \"recipe\""
  parseJSON x = typeMismatch "ingredient" x

instance ToJSON Ingredient where
  toJSON (Ingredient (Left ingredient) quantity) =
    object [ "ingredient" .= ingredient
           , "quantity" .= quantity
           ]
  toJSON (Ingredient (Right recipe) quantity) =
    object [ "recipe" .= recipe
           , "quantity" .= quantity
           ]

-- instance ToRow Ingredient where
--   toRow (Ingredient (Left name) quantity) = [ toField name
--                                             , toField Null
--                                             , toField (view quantityId quantity)]
--   toRow (Ingredient (Right recipe) quantity) = [ toField Null
--                                                , toField (view recipeId recipe)
--                                                , toField (view quantityId quantity)]

-- | A list of ingredients, along with text instructions.
data Recipe = Recipe
              { _recipeName         :: T.Text
              , _recipeIngredients  :: [Ingredient]
              , _recipeInstructions :: T.Text
              , _recipeId           :: Maybe Int
              }
            deriving (Show, Ord, Eq)

instance FromJSON Recipe where
  parseJSON (Object v) = Recipe <$> name <*> ingredients <*> instructions <*> _id
    where
      name         = v .:  "name"
      ingredients  = v .:  "ingredients"
      instructions = v .:  "instructions"
      _id          = v .:? "id"
  parseJSON x          = typeMismatch "recipe" x

instance ToJSON Recipe where
  toJSON (Recipe name ingredients instructions (Just _id)) =
    object [ "name"         .= name
           , "ingredients"  .= ingredients
           , "instructions" .= instructions
           , "id"           .= _id
           ]
  toJSON (Recipe name ingredients instructions Nothing) =
    object [ "name"         .= name
           , "ingredients"  .= ingredients
           , "instructions" .= instructions
           ]
    
-- instance ToRow Recipe where
--   toRow (Recipe name ingredients instructions (Just _id)) = [ toField name
--                                                             , toField ingredients
    
