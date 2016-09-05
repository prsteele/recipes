{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Recipes.DB_Test where

import           Control.Lens
import           Control.Monad.Reader
import qualified Data.Vector as V
import           Database.PostgreSQL.Simple
import           Test.Framework
import           Test.QuickCheck.Monadic

import Recipes.Arbitrary ()
import Recipes.DB
import Recipes.Types

connection :: IO Connection
connection = connect defaultConnectInfo { connectDatabase = "recipes_test"
                                        , connectUser     = "recipes_test"
                                        , connectHost     = "localhost"
                                        }

destroyQuery :: Query
destroyQuery = "DROP SCHEMA IF EXISTS public CASCADE"

createPublicSchema :: Query
createPublicSchema = "CREATE SCHEMA PUBLIC"

setup :: IO ()
setup = do
  conn <- connection
  _ <- execute conn destroyQuery ()
  _ <- execute conn createPublicSchema ()
  createTables conn

writeRead :: (DBStorable k t, DBRetrievable k t, Monad (m IO), MonadTrans m) =>
             t -> m IO (Maybe t)
writeRead x = do
  conn <- lift connection
  lift (runReaderT (store x >>= retrieve . fst) conn)

class Cleanable a where
  clean :: a -> a

instance Cleanable Ingredient where
  clean = ingredientId .~ Nothing

instance Cleanable Component where
  clean (IngredientComponent quantity ingredient _) =
    IngredientComponent (clean quantity) (clean ingredient) Nothing
  clean (RecipeComponent quantity recipe _) =
    RecipeComponent (clean quantity) (clean recipe) Nothing

instance Cleanable Recipe where
  clean (Recipe name components instructions _) =
    Recipe name (V.map clean components) instructions Nothing

instance Cleanable Quantity where
  clean (Quantity name amount) = Quantity name rounded
    where
      factor = 10 ** 6
      scaled = floor (amount * factor) :: Integer
      rounded = fromIntegral scaled / factor
          
prop_writeReadIngredient :: Ingredient -> Property
prop_writeReadIngredient ingredient = monadicIO $ do
  mIngredient <- writeRead ingredient
  assert $ case mIngredient of
    Nothing          -> False
    Just ingredient' -> clean ingredient == clean ingredient'
    
prop_writeReadComponent :: Component -> Property
prop_writeReadComponent component = monadicIO $ do
  mComponent <- writeRead component
  assert $ case mComponent of
    Nothing         -> False
    Just component' -> clean component == clean component'

prop_writeReadRecipe :: Recipe -> Property
prop_writeReadRecipe recipe = monadicIO $ do
  mRecipe <- writeRead recipe
  assert $ case mRecipe of
    Nothing      -> False
    Just recipe' -> clean recipe == clean recipe'
