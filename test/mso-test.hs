{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified Data.FormulaTest

import Test.Tasty
import Test.Tasty.TH

main :: IO ()
main = $(defaultMainGenerator)

test_all :: [TestTree]
test_all =
  [ Data.FormulaTest.tests
  ]
