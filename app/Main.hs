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

autosolveFixed :: IO ()
autosolveFixed = autosolveSecret $ Code G B BL WH

autosolveRandom :: IO ()
autosolveRandom = generateSecret >>= autosolveSecret

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
  case flags of
    [All] -> autosolveAll
    []    -> autosolveRandom
    _     -> putStrLn $ usageInfo header options
