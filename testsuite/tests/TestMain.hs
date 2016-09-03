{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import Test.Framework

import {-@ HTF_TESTS @-} Recipes.Types_Test
import {-@ HTF_TESTS @-} Recipes.DB_Test

main :: IO ()
main = setup >> htfMain htf_importedTests
