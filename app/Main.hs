module Main where

import           Mastermind
import           System.Console.GetOpt
import           System.Environment    (getArgs)

autosolveSecret secret = do
  putStrLn $ "secret: " ++ show secret
  let guesses = autosolve (evaluateGuess secret)
  putStrLn $ "Number of guesses: " ++ show (length guesses)
  let showGuess guess = "guess: " ++ show (fst guess) ++ " score: " ++ show (snd guess)
  mapM_ (putStrLn . showGuess) guesses

autosolveAll :: IO ()
autosolveAll = mapM_ autosolveSecret allCodes

autosolveOne :: IO ()
autosolveOne = do
  let secret = Code G B BL WH
  autosolveSecret secret

data Flag = All | Help deriving (Eq, Show)

options :: [OptDescr Flag]
options =
  [ Option ['a']     ["all"]  (NoArg All)  "exhaustively autosolve all secrets"
  , Option ['h','?'] ["help"] (NoArg Help) "display this usage information"
  ]

header = "Usage: mastermind [OPTION]"

parseArgs :: [String] -> IO [Flag]
parseArgs argv =
 case getOpt Permute options argv of
    (flags, _, []) -> return flags
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))

main :: IO ()
main = do
  argv <- getArgs
  flags <- parseArgs argv
  if Help `elem` flags
    then putStrLn $ usageInfo header options
    else if All `elem` flags
      then autosolveAll
      else autosolveOne
