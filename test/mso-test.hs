{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified Some.ModuleTest

import Test.Tasty
import Test.Tasty.TH

main :: IO ()
main = $(defaultMainGenerator)

test_all :: [TestTree]
test_all =
  [ Some.ModuleTest.tests
  ]
