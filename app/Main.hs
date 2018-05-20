module Main where

import           Control.Monad
import           Control.Parallel.Strategies
import           Data.List
import           Data.List.Split             (chunksOf)
import           Data.Maybe                  (fromMaybe)
import           Mastermind
import           System.Console.GetOpt
import           System.Environment          (getArgs)

autosolveSecret secret = do
  putStrLn $ "secret: " ++ show secret
  let guesses = autosolve (evaluateGuess secret)
  putStrLn $ "Number of guesses: " ++ show (length guesses)
  let showGuess guess = "guess: " ++ show (fst guess) ++ " score: " ++ show (snd guess)
  mapM_ (putStrLn . showGuess) guesses

autosolveWithSummary :: Code -> String
autosolveWithSummary secret = do
  let guesses = autosolve (evaluateGuess secret)
  "secret: " ++ show secret ++ " => #guesses: " ++ show (length guesses)

autosolveAll :: IO ()
autosolveAll =
  putStr ls
  where
    ls = unlines $ concat rs
    rs = map f chunks `using` parList rdeepseq
    f = map autosolveWithSummary
    chunks = chunksOf 36 allCodes

autosolveRandom :: IO ()
autosolveRandom = generateSecret >>= autosolveSecret

data Flag = All | Help | Secret Code deriving (Eq, Show)

stringToCodeFlag :: String -> Flag
stringToCodeFlag s =
  Secret code
  where
    code = Code p0 p1 p2 p3
    -- TODO: need to validate 's' (4 x numbers 0-5)
    -- TODO: ideally, don't expose intToPeg from Lib
    [p0, p1, p2, p3] = map (fromMaybe R . intToPeg . read . (:[])) s

options :: [OptDescr Flag]
options =
  [ Option ['a']     ["all"]    (NoArg All)                        "exhaustively autosolve all secrets"
  , Option ['s']     ["secret"] (ReqArg stringToCodeFlag "secret") "autosolve the given secret e.g. 1245"
  , Option ['h','?'] ["help"]   (NoArg Help)                       "display this usage information"
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
    [All]           -> autosolveAll
    [Secret secret] -> autosolveSecret secret
    []              -> autosolveRandom
    _               -> putStrLn $ usageInfo header options
