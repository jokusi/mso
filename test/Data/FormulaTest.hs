{-# LANGUAGE TemplateHaskell #-}
module Data.FormulaTest (tests) where

import Data.Formula

import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

tests :: TestTree
tests = $(testGroupGenerator)

data Var = P | P' deriving (Show, Read, Eq, Enum, Bounded)
data Status = Idle | Wait | Crit | Idle' | Wait' | Crit'
  deriving (Show, Read, Eq, Enum, Bounded)
data Gen1 = Gen1 (Var -> Int)
data Gen2 = Gen2 (Status -> [Int])

instance Arbitrary Gen1 where
  arbitrary = do
    let range = (0,1)
    p <- choose range
    p' <- choose range
    let f P  = p
        f P' = p'
    return $ Gen1 f

instance Arbitrary Gen2 where
  arbitrary = do
    let range :: [Int]
        range = [0,1]
    idle <- sublistOf range
    wait <- sublistOf range
    crit <- sublistOf range
    idle' <- sublistOf range
    wait' <- sublistOf range
    crit' <- sublistOf range
    let f Idle = idle
        f Wait = wait
        f Crit = crit
        f Idle' = idle'
        f Wait' = wait'
        f Crit' = crit'
    return $ Gen2 f

instance Show Gen1 where
  show (Gen1 f) = "Gen1 { P -> "
    ++ show (f P)
    ++ "; P' -> "
    ++ show (f P')
    ++ "}"

instance Show Gen2 where
  show (Gen2 f) = "Gen1 { Idle -> "
    ++ show (f Idle)
    ++ "; Wait -> "
    ++ show (f Wait)
    ++ "; Crit -> "
    ++ show (f Crit)
    ++ "; Idle' -> "
    ++ show (f Idle')
    ++ "; Wait' -> "
    ++ show (f Wait')
    ++ "; Crit' -> "
    ++ show (f Crit')
    ++ "}"

blocking :: Var -> Formula Var Status
blocking p = Ex (Var1 P') (P' `In` Crit :| (p :< P' :& P' `In` Wait))

trans :: Var -> Formula Var Status
trans p
  =  (p `In` Idle :& p `In` Idle' :=> Ex (Var1 P') (P' `In` Wait))
  :& (p `In` Idle :& p `In` Wait'
    :=> All (Var1 P') (p :/= P' :=> P' `In` Idle'))
  :& (p `In` Idle :& p `In` Crit' :=> FFalse)
  :& (p `In` Wait :& p `In` Idle' :=> FFalse)
  :& (p `In` Wait :& p `In` Wait' :=> blocking p)
  :& (p `In` Wait :& p `In` Crit' :=> Neg (blocking p))
  :& (p `In` Crit :& p `In` Idle' :=> FTrue)
  :& (p `In` Crit :& p `In` Wait' :=> FFalse)
  :& (p `In` Crit :& p `In` Crit' :=> FFalse)

safety, safety' :: Var -> Var -> Formula Var Status
safety p p'  = Neg (p `In` Crit :& p' `In` Crit)
safety' p p' = Neg (p `In` Crit' :& p' `In` Crit')

phiSafety :: Formula Var Status
phiSafety = P :/= P' :=> safety P P' :& trans P :& trans P' :=> safety' P P'

prop_safety :: Gen1 -> Gen2 -> Bool
prop_safety (Gen1 v1) (Gen2 v2) = eval {-numer of processes-}2 v1 v2 phiSafety

