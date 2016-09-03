-- | Utilities related to writing to, reading from, and maintaining
-- the application database.
module Recipes.DB where

import Control.Monad.Reader
import Database.PostgreSQL.Simple

import qualified Recipes.DB.Queries as Q
import Recipes.Types

-- | An environment holding a database connection.
type DB = ReaderT Connection IO

-- | Create all database tables
createTables :: Connection -> IO ()
createTables conn = mapM_ (\q -> execute conn q ()) queries
  where
    queries = [ Q.createIngredients
              , Q.createRecipes
              , Q.createComponents
              ]

insertIngredient :: Ingredient -> DB Integer
insertIngredient (Ingredient name Nothing) = do
  conn       <- ask
  [Only _id] <- liftIO $ query conn Q.insertIngredient (Only name)
  return $ _id
insertIngredient (Ingredient _ (Just _id)) = return _id

readIngredient :: Integer -> DB (Maybe Ingredient)
readIngredient _id = do
  conn    <- ask
  results <- liftIO $ query conn Q.readIngredient (Only _id)
  case results of
    [ingredient] -> return (Just ingredient)
    _            -> return Nothing
