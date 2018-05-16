module Mastermind (
  Peg(..),
  Code(..),
  Score(..),
  evaluateGuess,
  autosolve
) where

data Peg = R | G | B | Y | BL | WH deriving (Eq, Show)
data Code = Code { p0 :: Peg, p1 :: Peg, p2 :: Peg, p3 :: Peg } deriving (Eq, Show)
data Score = Score { blacks :: Int, whites :: Int } deriving (Eq, Show)

allPegs :: [Peg]
allPegs = [R, G, B, Y, BL, WH]

pegs :: Code -> [Peg]
pegs code = [p0 code, p1 code, p2 code, p3 code]

evaluateGuess :: Code -> Code -> Score
evaluateGuess secret guess =
  Score { blacks = blacks, whites = whites }
  where
    spegs = pegs secret
    gpegs = pegs guess
    countMatchingPegs p pegs = length $ filter (== p) pegs
    minPegs p = min (countMatchingPegs p spegs) (countMatchingPegs p gpegs)
    sumOfMins = sum $ map minPegs allPegs
    blacks = length $ filter (uncurry (==)) $ zip spegs gpegs
    whites = sumOfMins - blacks

autosolve :: (Code -> Score) -> [(Code, Score)]
autosolve attempt =
  []
