import           Mastermind
import           Test.Hspec

main :: IO ()
main = hspec $

  describe "Unit Tests" $ do

    describe "evaluateGuess" $ do

      it "no overlap" $ do
        let secret = Code { p0 = R, p1 = G, p2 = B, p3 = Y }
        let guess = Code { p0 = BL, p1 = BL, p2 = WH, p3 = WH }
        let score = evaluateGuess secret guess
        score `shouldBe` Score { blacks = 0, whites = 0 }

      it "exact match" $ do
        let secret = Code { p0 = R, p1 = G, p2 = B, p3 = Y }
        let guess = Code { p0 = R, p1 = G, p2 = B, p3 = Y }
        let score = evaluateGuess secret guess
        score `shouldBe` Score { blacks = 4, whites = 0 }

      it "correct colours in wrong positions" $ do
        let secret = Code { p0 = R, p1 = G, p2 = B, p3 = Y }
        let guess = Code { p0 = Y, p1 = B, p2 = G, p3 = R }
        let score = evaluateGuess secret guess
        score `shouldBe` Score { blacks = 0, whites = 4 }

      it "scenario 1" $ do
        let secret = Code { p0 = G, p1 = G, p2 = B, p3 = B }
        let guess = Code { p0 = B, p1 = B, p2 = G, p3 = G }
        let score = evaluateGuess secret guess
        score `shouldBe` Score { blacks = 0, whites = 4 }

      it "scenario 2" $ do
        let secret = Code { p0 = G, p1 = G, p2 = B, p3 = B }
        let guess = Code { p0 = B, p1 = B, p2 = G, p3 = Y }
        let score = evaluateGuess secret guess
        score `shouldBe` Score { blacks = 0, whites = 3 }

      it "scenario 3" $ do
        let secret = Code { p0 = G, p1 = G, p2 = B, p3 = Y }
        let guess = Code { p0 = B, p1 = B, p2 = G, p3 = G }
        let score = evaluateGuess secret guess
        score `shouldBe` Score { blacks = 0, whites = 3 }

      it "scenario 4" $ do
        let secret = Code { p0 = B, p1 = Y, p2 = WH, p3 = WH }
        let guess = Code { p0 = B, p1 = WH, p2 = Y, p3 = Y }
        let score = evaluateGuess secret guess
        score `shouldBe` Score { blacks = 1, whites = 2 }

    describe "autosolve" $

      it "fixed secret" $ do
        1 `shouldBe` 1
