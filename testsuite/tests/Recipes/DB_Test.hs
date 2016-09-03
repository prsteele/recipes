{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Recipes.DB_Test where

import Control.Lens
import Control.Monad
import Control.Monad.Reader
import Database.PostgreSQL.Simple
import Test.Framework
import Test.QuickCheck.Monadic

import Recipes.Arbitrary ()
import Recipes.DB
import Recipes.Types

connection :: IO Connection
connection = connect defaultConnectInfo { connectDatabase = "recipes_test"
                                        , connectUser = "recipes_test"
                                        , connectHost = "localhost"
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

writeRead :: Ingredient -> DB (Maybe Ingredient)
writeRead ingredient = insertIngredient ingredient >>= readIngredient

prop_writeReadIngredient :: Ingredient -> Property
prop_writeReadIngredient ingredient = monadicIO $ do
  conn   <- lift connection
  mingredient <- lift (runReaderT (writeRead ingredient) conn)

  let result = case mingredient of
        Nothing          -> False
        Just ingredient' -> set ingredientId Nothing ingredient' == ingredient

  when (not result) $ do
    lift $ print ("---------------------" :: String)
    lift $ print mingredient
    lift $ print ingredient
    lift $ print ("---------------------" :: String)

  assert result

test_create :: IO ()
test_create = connection >>= createTables
