module MiguelSpec where

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes
import MiguelC21

miguelTests = do
    describe "Miguel Tests" $ do
        it "chapter 21" miguelsChapter21

