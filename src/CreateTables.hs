{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.ByteString as B
import Database.PostgreSQL.Simple

import Recipes.DB

connectionStr :: B.ByteString
connectionStr = "user=recipes dbname=recipes"

main :: IO ()
main = connectPostgreSQL connectionStr >>= createTables
