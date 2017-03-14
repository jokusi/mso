module Some.Module
  ( someFunction
  , someIO
  ) where

someFunction :: Int -> Int
someFunction = (*2)

someIO :: IO ()
someIO = putStrLn "Hello, world!"
