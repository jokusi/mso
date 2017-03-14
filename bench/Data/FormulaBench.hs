module Data.FormulaBench (benchs) where

import Data.Formula

import Criterion.Main
benchs :: Benchmark
benchs = bgroup "Data.Formula"
  [
  ]

{-
bench_someFunction :: Benchmark
bench_someFunction = bgroup "someFunction"
  [ bench "1" $ nf someFunction 1
  , bench "2" $ nf someFunction 2
  , bench "100" $ nf someFunction 100
  ]

bench_someIO :: Benchmark
bench_someIO = bench "someIO" $ whnfIO someIO
-}
