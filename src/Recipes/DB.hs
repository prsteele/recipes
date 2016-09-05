{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
-- | Utilities related to writing to, reading from, and maintaining
-- the application database.
module Recipes.DB where

import           Control.Lens
import           Control.Monad.Reader
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Vector as V
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow

import qualified Recipes.DB.Queries as Q
import           Recipes.Types

-- | An environment holding a database connection.
type DB = ReaderT Connection IO

-- | A DBStorable is a type 't' that can be stored in the database,
-- indexed by a key of type 'k'.
class DBStorable k t | t -> k where
  store :: t -> DB (k, t)

-- | A DBStorable is a type 't' that can be retrieved from the
-- database with a key of type 'k'.
class DBRetrievable k t | k -> t where
  retrieve :: k -> DB (Maybe t)

instance DBStorable IngredientId Ingredient where
  store x@(Ingredient _ (Just iid)) = return (iid, x)
  store (Ingredient name Nothing) = do
    conn       <- ask
    [Only iid] <- liftIO $ query conn Q.insertIngredient (Only name)
    return (iid, Ingredient name (Just iid))

instance DBRetrievable IngredientId Ingredient where
  retrieve (IngredientId _id) = do
    conn    <- ask
    results <- liftIO $ query conn Q.readIngredient (Only _id)
    case results of
      [ingredient] -> return (Just ingredient)
      _            -> return Nothing

-- | A helper structure to store the results of querying a
-- component.
--
-- We store both recipe and ingredient Components in the same table,
-- so we need to retrieve information about the Component before we
-- can query its sub-fields.
data ComponentDescriptor = ComponentDescriptor
                           { _componentDescriptorIsIngredient :: Bool
                           , _componentDescriptorReference    :: Integer
                           , _componentDescriptorQuantity     :: Quantity
                           }

instance FromRow ComponentDescriptor where
  fromRow = do
    isIngredient <- field
    ref          <- field
    amount       <- field
    name         <- field
    return $ ComponentDescriptor isIngredient ref (Quantity name amount)
      
instance DBStorable ComponentId Component where
  store c@(IngredientComponent _ ingredient _) = do
    (iid, ingredient') <- store ingredient

    let amount = c ^. componentQuantity . quantityAmount
        unit   = c ^. componentQuantity . quantityName
        args   = (iid, amount, unit)

    case c ^. componentId of
      (Just cid) -> return (cid, c & componentIngredient .~ ingredient')
      Nothing    -> do
        conn       <- ask
        [Only cid] <- liftIO $ query conn Q.insertIngredientComponent args
        return (cid, c & componentIngredient .~ ingredient & componentId .~ Just cid)
  store c@(RecipeComponent _ recipe _) = do
    (rid, recipe') <- store recipe

    let amount = c ^. componentQuantity . quantityAmount
        unit   = c ^. componentQuantity . quantityName
        args   = (rid, amount, unit)

    case c ^. componentId of
      (Just cid) -> return (cid, c & componentRecipe .~ recipe')
      Nothing    -> do
        conn <- ask
        [Only cid] <- liftIO $ query conn Q.insertRecipeComponent args
        return (cid, c & componentRecipe .~ recipe & componentId .~ Just cid)

instance DBRetrievable ComponentId Component where
  retrieve cid =
    let
      retrieve' (ComponentDescriptor True ref q) =
        retrieve'' IngredientComponent (IngredientId ref) q
      retrieve' (ComponentDescriptor False ref q) =
        retrieve'' RecipeComponent (RecipeId ref) q

      retrieve'' con sid quantity = do
        mSub <- retrieve sid
        return $ case mSub of
          Just x  -> Just (con quantity x (Just cid))
          Nothing -> Nothing
    in do
      conn    <- ask
      results <- liftIO $ query conn Q.readComponentDescriptor (Only cid)
      case results of
        [descriptor] -> retrieve' descriptor
        _            -> return Nothing

-- | A helper structure to store the results of querying a
-- Recipe.
--
-- We receive back a list of Component IDs that we must query in turn.
data RecipeDescriptor = RecipeDescriptor
                        { _recipeDescriptorName         :: T.Text
                        , _recipeDescriptorComponentIds :: V.Vector ComponentId
                        , _recipeDescriptorInstructions :: T.Text
                        }

instance FromRow RecipeDescriptor where
  fromRow = do
    name         <- field
    componentIds <- field
    instructions <- field
    return $ RecipeDescriptor name (V.map ComponentId componentIds) instructions

instance DBStorable RecipeId Recipe where
  store r = do
    pairs <- V.mapM store (r ^. recipeComponents)

    let name         = r ^. recipeName
        instructions = r ^. recipeInstructions
        componentIds = V.map fst pairs
        components   = V.map snd pairs
        args         = (name, componentIds, instructions)

    case r ^. recipeId of
      Just (rid) -> return (rid, r & recipeComponents .~ components)
      Nothing    -> do
        conn       <- ask
        [Only rid] <- liftIO $ query conn Q.insertRecipe args
        return (rid, r & recipeComponents .~ components & recipeId .~ Just rid)

instance DBRetrievable RecipeId Recipe where
  retrieve rid =
    let
      retrieve' (RecipeDescriptor name componentIds instructions) = do
        components <- V.mapM retrieve componentIds
        let found = V.fromList (catMaybes (V.toList components))
        return $ Just (Recipe name found instructions (Just rid))
    in do
      conn    <- ask
      results <- liftIO $ query conn Q.readRecipeDescriptor (Only rid)
      case results of
        [descriptor] -> retrieve' descriptor
        _            -> return Nothing

-- | Create all database tables
createTables :: Connection -> IO ()
createTables conn = mapM_ (\q -> execute conn q ()) queries
  where
    queries = [ Q.createIngredients
              , Q.createRecipes
              , Q.createComponents
              ]
