{-# LANGUAGE TemplateHaskell #-}
module Some.ModuleTest (tests) where

import Some.Module

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

tests :: TestTree
tests = $(testGroupGenerator)

prop_twice :: Int -> Bool
prop_twice i = someFunction i == (2 * i)

case_twenty :: Assertion
case_twenty = someFunction 10 @?= 20
