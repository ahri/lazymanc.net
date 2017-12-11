module ChrisAddition where

import Test.Hspec
import Test.QuickCheck

genInt :: Gen Int
genInt = elements [1, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)

myMul :: (Eq a, Num a) => a -> a -> a
myMul n 0 = 0
myMul n m = n + myMul n (m-1)

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

chrisMain :: IO ()
chrisMain = hspec $ do
    describe "Addition" $ do
        it "15 divided by 5 is 3" $ do
            dividedBy 15 5 `shouldBe` (3, 0)
        it "22 divided by 5 is 4 remainder 2" $ do
            dividedBy 22 5 `shouldBe` (4, 2)
        it "4 * 5 is 20" $ do
            myMul 4 5 `shouldBe` 20
        it "x + 1 is always greater than x" $ do
            property $ \x -> x + 1 > (x :: Int)

chrisRunQc :: IO ()
chrisRunQc = quickCheck prop_additionGreater
