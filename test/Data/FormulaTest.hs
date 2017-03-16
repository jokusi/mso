{-# LANGUAGE TemplateHaskell #-}
module Data.FormulaTest (tests) where

import Data.Formula

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

tests :: TestTree
tests = $(testGroupGenerator)

data Var = P | P' deriving (Show, Read, Eq)
data Status = Idle | Wait | Crit | Idle' | Wait' | Crit'
  deriving (Show, Read, Eq)

blocking :: Var -> Formula Var Status
blocking p = Ex (Var1 P') (P' `In` Crit :| (p :< P' :& P' `In` Wait))

trans :: Var -> Formula Var Status
trans p
  =  (p `In` Idle :& p `In` Idle' :=> Ex (Var1 P') (P' `In` Wait))
  :& (p `In` Idle :& p `In` Wait' :=> All (Var1 P') (p :/= P' :=> P' `In` Idle'))
  :& (p `In` Idle :& p `In` Crit' :=> FFalse)
  :& (p `In` Wait :& p `In` Idle' :=> FFalse)
  :& (p `In` Wait :& p `In` Wait' :=> blocking p)
  :& (p `In` Wait :& p `In` Crit' :=> Neg (blocking p))
  :& (p `In` Crit :& p `In` Idle' :=> FTrue)
  :& (p `In` Crit :& p `In` Wait' :=> FFalse)
  :& (p `In` Crit :& p `In` Crit' :=> FFalse)

safety, safety' :: Var -> Var -> Formula Var Status
safety p p'  = p `In` Crit :& p' `In` Crit
safety' p p' = p `In` Crit' :& p' `In` Crit'

phiSafety :: Formula Var Status
phiSafety = safety P P' :& trans P :& trans P' :=> safety' P P'

case_safety :: Assertion
case_safety = assert $ eval 2 store1 store2 phiSafety where
  store1 P  = 0
  store1 P' = 1

  store2 Idle = [0,1]
  store2 Idle' = [1]
  store2 Wait' = [0]
  store2 _     = []
