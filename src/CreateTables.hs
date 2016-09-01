{-# LANGUAGE OverloadedStrings #-}
module Main where

import Database.PostgreSQL.Simple

tmp = do
  conn <- connectPostgreSQL "user=recipes dbname=recipes"
  execute conn "CREATE TABLE IF NOT EXISTS foo (fooid SERIAL PRIMARY KEY, bar BIGINT)" ()

main = tmp >>= print
