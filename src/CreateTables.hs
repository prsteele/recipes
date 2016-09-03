{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.ByteString as B
import Database.PostgreSQL.Simple

createIngredientsQuery :: Query
createIngredientsQuery =
  "CREATE TABLE IF NOT EXISTS Ingredients (\
  \IngredientId  BIGSERIAL PRIMARY KEY,\
  \Name          TEXT NOT NULL\
  \)"

createRecipesQuery :: Query
createRecipesQuery =
  "CREATE TABLE IF NOT EXISTS Recipes (\
  \RecipeId      BIGSERIAL PRIMARY KEY,\
  \Name          TEXT NOT NULL,\
  \Components    BIGINT[] NOT NULL,\
  \Instructions  TEXT NOT NULL\
  \)"

createComponentsQuery :: Query
createComponentsQuery =
  "CREATE TABLE IF NOT EXISTS Components (\
  \ComponentId   BIGSERIAL PRIMARY KEY,\
  \IsIngredient  BOOLEAN NOT NULL,\
  \Ingredient    BIGINT REFERENCES Ingredients (IngredientId),\
  \Recipe        BIGINT REFERENCES Recipes (RecipeId),\
  \Quantity      DOUBLE PRECISION NOT NULL,\
  \Unit          TEXT\
  \)"

createTables :: Connection -> IO ()
createTables conn = mapM_ (\q -> execute conn q ()) queries
  where
    queries = [ createIngredientsQuery
              , createRecipesQuery
              , createComponentsQuery
              ]

connectionStr :: B.ByteString
connectionStr = "user=recipes dbname=recipes"

main :: IO ()
main = connectPostgreSQL connectionStr >>= createTables
