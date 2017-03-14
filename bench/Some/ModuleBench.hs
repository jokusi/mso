module Some.ModuleBench (benchs) where

import Some.Module

import Criterion.Main

benchs :: Benchmark
benchs = bgroup "Some.Module"
  [ bench_someFunction
  , bench_someIO
  ]

bench_someFunction :: Benchmark
bench_someFunction = bgroup "someFunction"
  [ bench "1" $ nf someFunction 1
  , bench "2" $ nf someFunction 2
  , bench "100" $ nf someFunction 100
  ]

bench_someIO :: Benchmark
bench_someIO = bench "someIO" $ whnfIO someIO
