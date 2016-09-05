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
  \ComponentId         BIGSERIAL PRIMARY KEY,\
  \IsIngredient        BOOLEAN NOT NULL,\
  \IngredientOrRecipe  BIGINT,\
  \Quantity            DOUBLE PRECISION NOT NULL,\
  \Unit                TEXT\
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

insertIngredientComponent :: Query
insertIngredientComponent =
  "INSERT INTO Components (\
  \IsIngredient,\
  \IngredientOrRecipe,\
  \Quantity,\
  \Unit\
  \) VALUES (\
  \TRUE, ?, ?, ?\
  \)\
  \RETURNING ComponentId"

insertRecipeComponent :: Query
insertRecipeComponent =
  "INSERT INTO Components (\
  \IsIngredient,\
  \IngredientOrRecipe,\
  \Quantity,\
  \Unit\
  \) VALUES (\
  \FALSE, ?, ?, ?\
  \)\
  \RETURNING ComponentId"

readComponentDescriptor :: Query
readComponentDescriptor =
  "SELECT \
  \IsIngredient,\
  \IngredientOrRecipe,\
  \Quantity,\
  \Unit \
  \FROM Components \
  \WHERE ComponentId = ?"

insertRecipe :: Query
insertRecipe =
  "INSERT INTO Recipes (\
  \Name,\
  \Components,\
  \Instructions\
  \) VALUES (\
  \?, ?, ?\
  \)\
  \RETURNING RecipeId"

readRecipeDescriptor :: Query
readRecipeDescriptor =
  "SELECT \
  \Name,\
  \Components,\
  \Instructions \
  \FROM Recipes \
  \WHERE RecipeId = ?"
