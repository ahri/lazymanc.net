{-# LANGUAGE ScopedTypeVariables #-}

import qualified C08
import qualified C17
import           ChrisAddition
import           ChrisMonoidLaws
import           Lib
import           MiguelSpec
import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

main :: IO ()
main = do
    chrisMain
    chrisRunQc
    chrisMonoidLawsTest
    hspec $ do
        describe "Multiplication" $ do
            it "5 * 1 == 5" $ do
                C08.mult 5 1 `shouldBe` 5

            it "5 * 2 == 10" $ do
                C08.mult 5 2 `shouldBe` 10

            it "identity is 1" $ do
                property $ \(x :: Int) -> C08.mult x 1 == x

            it "demo checkers library for monoid" $ do
                quickBatch $ monoid (undefined :: [String])

            it "demo checkers library for monoid List" $ do
                quickBatch $ monoid (undefined :: C17.List String)

            it "demo checkers library for applicative" $ do
                quickBatch $ applicative (undefined :: [(String, String, Int)])

            it "demo checkers library for applicative List" $ do
                quickBatch $ applicative (undefined :: C17.List (String, String, Int))
        miguelTests

    chapters
