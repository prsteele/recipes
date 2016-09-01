{-# OPTIONS_GHC -F -pgmF htfpp #-}
{-# LANGUAGE OverloadedStrings #-}
module Recipes_Test where

import Test.Framework
import qualified Data.Text as T

import Recipes
import Data.Aeson

instance Arbitrary Quantity where
  arbitrary = Quantity <$> fmap T.pack arbitrary <*> arbitrary

prop_quantityToFrom :: Quantity -> Bool
prop_quantityToFrom q = case decode (encode q) of
  Just q' -> q == q'
  Nothing -> False
