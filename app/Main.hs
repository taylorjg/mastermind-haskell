module Main where

import           Mastermind

main :: IO ()
main = do
  let c1 = Code { p0 = R, p1 = R, p2 = G, p3 = G }
  let c2 = Code { p0 = R, p1 = R, p2 = G, p3 = G }
  putStrLn "Hello, Haskell!"
