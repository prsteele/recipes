{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Recipes.DB_Test where

import Database.PostgreSQL.Simple
import Test.Framework

import Recipes.DB

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

test_create :: IO ()
test_create = connection >>= createTables
