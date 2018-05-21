import           Mastermind
import           Test.Hspec

main :: IO ()
main = hspec $

  describe "Unit Tests" $ do

    describe "evaluateGuess" $ do

      it "no overlap" $ do
        let secret = Code R G B Y
        let guess = Code BL BL WH WH
        let score = evaluateGuess secret guess
        score `shouldBe` Score 0 0

      it "exact match" $ do
        let secret = Code R G B Y
        let guess = Code R G B Y
        let score = evaluateGuess secret guess
        score `shouldBe` Score 4 0

      it "correct colours in wrong positions" $ do
        let secret = Code R G B Y
        let guess = Code Y B G R
        let score = evaluateGuess secret guess
        score `shouldBe` Score 0 4

      it "scenario 1" $ do
        let secret = Code G G B B
        let guess = Code B B G G
        let score = evaluateGuess secret guess
        score `shouldBe` Score 0 4

      it "scenario 2" $ do
        let secret = Code G G B B
        let guess = Code B B G Y
        let score = evaluateGuess secret guess
        score `shouldBe` Score 0 3

      it "scenario 3" $ do
        let secret = Code G G B Y
        let guess = Code B B G G
        let score = evaluateGuess secret guess
        score `shouldBe` Score 0 3

      it "scenario 4" $ do
        let secret = Code B Y WH WH
        let guess = Code B WH Y Y
        let score = evaluateGuess secret guess
        score `shouldBe` Score 1 2

    describe "autosolve" $ do

      it "fixed secret G B BL WH" $ do
        let secret = Code G B BL WH
        let guesses = autosolve (evaluateGuess secret)
        length guesses `shouldSatisfy` (<= 5)
        let lastPair = last guesses
        fst lastPair `shouldBe` secret
        snd lastPair `shouldBe` Score 4 0

      it "fixed secret R R G WH" $ do
        let secret = Code R R G WH
        let guesses = autosolve (evaluateGuess secret)
        length guesses `shouldSatisfy` (<= 5)
        let lastPair = last guesses
        fst lastPair `shouldBe` secret
        snd lastPair `shouldBe` Score 4 0
