module Main where

import           Mastermind

main :: IO ()
main = do
  let secret = Code G B BL WH
  putStrLn $ "secret: " ++ show secret
  let guesses = autosolve (evaluateGuess secret)
  putStrLn $ "Number of guesses: " ++ show (length guesses)
  let showGuess guess = "guess: " ++ show (fst guess) ++ " score: " ++ show (snd guess)
  mapM_ (putStrLn . showGuess) guesses
