module Main where

import qualified Data.FormulaBench

import Criterion.Main
import Criterion.Types

reportFilePath :: FilePath
reportFilePath = "mso-bench.html"

main :: IO ()
main = defaultMainWith defaultConfig{reportFile = Just reportFilePath}
  [ bgroup "all"
    [ Data.FormulaBench.benchs
    ]
  ]
