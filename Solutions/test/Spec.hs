{-# LANGUAGE ScopedTypeVariables #-}

import qualified C08
import           Lib
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = do
    chapters
    hspec $ do
        describe "Multiplication" $ do
            it "5 * 1 == 5" $ do
                C08.mult 5 1 `shouldBe` 5

            it "5 * 2 == 10" $ do
                C08.mult 5 2 `shouldBe` 10

            it "identity is 1" $ do
                property $ \(x :: Int) -> C08.mult x 1 == x
