-- | Utilities related to writing to, reading from, and maintaining
-- the application database.
module Recipes.DB where

import Database.PostgreSQL.Simple

import Recipes.DB.Queries

-- | Create all database tables
createTables :: Connection -> IO ()
createTables conn = mapM_ (\q -> execute conn q ()) queries
  where
    queries = [ createIngredientsQuery
              , createRecipesQuery
              , createComponentsQuery
              ]
