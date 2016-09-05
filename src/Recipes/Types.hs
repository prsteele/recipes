{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Recipes.Types where

import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import qualified Data.Vector as V
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToField
import GHC.Generics

-- | The type for Recipe IDs in the database.
newtype RecipeId = RecipeId Integer
                 deriving (Show, Ord, Eq, Generic)

instance FromJSON RecipeId
instance ToJSON RecipeId
instance FromField RecipeId where
  fromField f mb = RecipeId <$> fromField f mb
instance ToField RecipeId where
   toField (RecipeId rid) = toField rid

-- | The type for Component IDs in the database.
newtype ComponentId = ComponentId Integer
                    deriving (Show, Ord, Eq, Generic)

instance FromJSON ComponentId
instance ToJSON ComponentId
instance FromField ComponentId where
  fromField f mb = ComponentId <$> fromField f mb
instance ToField ComponentId where
  toField (ComponentId cid) = toField cid

-- | The type for Ingredient IDs in the database.
newtype IngredientId = IngredientId Integer
                     deriving (Show, Ord, Eq, Generic)

instance FromJSON IngredientId
instance ToJSON IngredientId
instance FromField IngredientId where
  fromField f mb = IngredientId <$> fromField f mb
instance ToField IngredientId where
  toField (IngredientId iid) = toField iid

-- | A Recipe consisting of a name, a number of components, and
-- instructions.
data Recipe = Recipe
              { _recipeName         :: T.Text
              , _recipeComponents   :: V.Vector Component
              , _recipeInstructions :: T.Text
              , _recipeId           :: Maybe RecipeId
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
                  , _ingredientId   :: Maybe IngredientId
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

instance FromRow Ingredient where
  fromRow = Ingredient <$> field <*> fmap (Just . IngredientId) field

-- | Either an Ingredient or a Recipe, along with a quantity.
data Component = IngredientComponent
                 { _componentQuantity   :: Quantity
                 , _componentIngredient :: Ingredient
                 , _componentId         :: Maybe ComponentId
                 }
               | RecipeComponent
                 { _componentQuantity :: Quantity
                 , _componentRecipe   :: Recipe
                 , _componentId       :: Maybe ComponentId
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

-- | The concrete database IDs of a Component.
--
-- This should be constructed by a call to insertComponent.
data ComponentIds = IngredientComponentIds
                    { _componentIdsComponentId  :: Integer
                    , _componentIdsIngredientId :: Integer
                    }
                  | RecipeComponentIds
                    { _componentIdsComponentId :: Integer
                    , _componentIdsRecipeId    :: Integer
                    }

makeLenses ''Quantity
makeLenses ''Ingredient
makeLenses ''Recipe
makeLenses ''Component
makeLenses ''ComponentIds
