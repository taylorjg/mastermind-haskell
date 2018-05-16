module Main where

import           Mastermind

main :: IO ()
main = do
  let c1 = Code R R G G
  let c2 = Code R R G G
  putStrLn "Hello, Haskell!"
