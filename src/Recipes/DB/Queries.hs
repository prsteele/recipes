{-# LANGUAGE OverloadedStrings #-}
-- | Hard-coded database queries.
module Recipes.DB.Queries where

import Database.PostgreSQL.Simple

createIngredients :: Query
createIngredients =
  "CREATE TABLE IF NOT EXISTS Ingredients (\
  \IngredientId  BIGSERIAL PRIMARY KEY,\
  \Name          TEXT NOT NULL\
  \)"

createRecipes :: Query
createRecipes =
  "CREATE TABLE IF NOT EXISTS Recipes (\
  \RecipeId      BIGSERIAL PRIMARY KEY,\
  \Name          TEXT NOT NULL,\
  \Components    BIGINT[] NOT NULL,\
  \Instructions  TEXT NOT NULL\
  \)"

createComponents :: Query
createComponents =
  "CREATE TABLE IF NOT EXISTS Components (\
  \ComponentId   BIGSERIAL PRIMARY KEY,\
  \IsIngredient  BOOLEAN NOT NULL,\
  \Ingredient    BIGINT REFERENCES Ingredients (IngredientId),\
  \Recipe        BIGINT REFERENCES Recipes (RecipeId),\
  \Quantity      DOUBLE PRECISION NOT NULL,\
  \Unit          TEXT\
  \)"

insertIngredient :: Query
insertIngredient =
  "INSERT INTO Ingredients (\
  \Name\
  \) VALUES (\
  \?\
  \)\
  \RETURNING IngredientId"

readIngredient :: Query
readIngredient =
  "SELECT \
  \Name,\
  \IngredientId \
  \FROM Ingredients \
  \WHERE IngredientId = ?"
