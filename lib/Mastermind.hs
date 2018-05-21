module Mastermind (
  Peg(..),
  Code(..),
  Score(..),
  generateSecret,
  evaluateGuess,
  autosolve,
  allCodes,
  intToPeg
) where

import           Data.Maybe    (fromMaybe)
import           System.Random

data Peg = R | G | B | Y | BL | WH deriving Eq
data Code = Code { p0 :: Peg, p1 :: Peg, p2 :: Peg, p3 :: Peg } deriving Eq
data Score = Score { blacks :: Int, whites :: Int } deriving Eq

instance Show Peg where
  show peg = case peg of
    R  -> "0"
    G  -> "1"
    B  -> "2"
    Y  -> "3"
    BL -> "4"
    WH -> "5"

intToPeg :: Int -> Maybe Peg
intToPeg n = case n of
    0 -> Just R
    1 -> Just G
    2 -> Just B
    3 -> Just Y
    4 -> Just BL
    5 -> Just WH
    _ -> Nothing

instance Read Peg where
  readsPrec _ r = case intToPeg $ read r of
    Just peg -> [(peg, "")]
    Nothing  -> []

instance Show Code where
  show code = concatMap show $ pegs code

instance Show Score where
  show score = show (blacks score) ++ show (whites score)

allPegs :: [Peg]
allPegs = [R, G, B, Y, BL, WH]

allCodes :: [Code]
allCodes = [Code p0 p1 p2 p3 | p0 <- allPegs, p1 <- allPegs, p2 <- allPegs, p3 <- allPegs]

allScores :: [Score]
allScores = [Score bs ws | bs <- [0..4], ws <- [0..4 - bs], not (bs == 3 && ws == 1)]

pegs :: Code -> [Peg]
pegs code = [p0 code, p1 code, p2 code, p3 code]

randomPeg :: IO Peg
randomPeg = fromMaybe R . intToPeg <$> getStdRandom (randomR (0, 5))

generateSecret :: IO Code
generateSecret = do
  p0 <- randomPeg
  p1 <- randomPeg
  p2 <- randomPeg
  p3 <- randomPeg
  return $ Code p0 p1 p2 p3

evaluateGuess :: Code -> Code -> Score
evaluateGuess secret guess =
  Score blacks whites
  where
    spegs = pegs secret
    gpegs = pegs guess
    countMatchingPegs p pegs = length $ filter (== p) pegs
    minPegs p = min (countMatchingPegs p spegs) (countMatchingPegs p gpegs)
    sumOfMins = sum $ map minPegs allPegs
    blacks = length $ filter (uncurry (==)) $ zip spegs gpegs
    whites = sumOfMins - blacks

autosolve :: (Code -> Score) -> [(Code, Score)]
autosolve attempt = reverse $ autosolve' attempt allCodes []

initialGuess :: Code
initialGuess = Code R R G G

autosolve' :: (Code -> Score) -> [Code] -> [(Code, Score)] -> [(Code, Score)]
autosolve' attempt set acc =
  if blacks score == 4 then acc' else autosolve' attempt set' acc'
  where
    guess = case (set, acc) of
      (_, [])     -> initialGuess
      ([code], _) -> code
      _           -> calculateNewGuess set
    score = attempt guess
    set' = filter ((score ==) . evaluateGuess guess) set
    acc' = (guess, score) : acc

calculateNewGuess :: [Code] -> Code
calculateNewGuess set =
  snd best
  where
    best = foldl op1 (maxBound :: Int, initialGuess) allCodes
    op1 currentBest unusedCode =
      if x < fst currentBest then (x, unusedCode) else currentBest
      where
        x = foldl op2 0 allScores
        op2 currentMax score =
          max currentMax y
          where y = length $ filter ((score ==) . evaluateGuess unusedCode) set
