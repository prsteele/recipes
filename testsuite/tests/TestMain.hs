{-# OPTIONS_GHC -F -pgmF htfpp #-}

module Main where

import Test.Framework

import {-@ HTF_TESTS @-} Recipes_Test

main :: IO ()
main = htfMain htf_importedTests
