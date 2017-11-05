module C8 where

import           Data.List (intersperse)

c8 :: IO ()
c8 = do
    print $ recSum 5
    print $ mult 3 5
    print $ mult 5 3
    print $ dividedBy 7 3
    print $ dividedBy 7 1
    print $ dividedBy 7 0
    print $ dividedBy 3 7
    print $ map mc91 [95..110]
    print $ wordNumber 12324546

recSum :: Integral a => a -> a
recSum 0 = 0
recSum n = n + recSum (n-1)

mult :: Integral a => a -> a -> a
mult 1 n = n
mult n 1 = n
mult n m
    | n < m = m + mult (n-1) m
    | otherwise = n + mult n (m-1)

data DividedResult a = Result a | DividedByZero deriving (Show, Eq)
dividedBy :: Integral a => a -> a -> DividedResult a
dividedBy _ 0 = DividedByZero
dividedBy n 1 = Result n
dividedBy n d = Result $ go n 0
    where go n' c
            | n' < d = c
            | otherwise = go (n'-d) (c+1)

mc91 :: Integral a => a -> a
mc91 x
    | x > 100 = x - 10
    | otherwise = mc91 . mc91 $ x + 11

wordNumber :: Int -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits'

digits :: Int -> [Int]
digits = map (read . charToString) . show
    where charToString = (:[])

digits' :: Int -> [Int]
digits' i
    | i < 10 = [i]
    | otherwise = (digits' . (`div` 10) $ i) ++ [mod i 10]

digitToWord :: Int -> String
digitToWord 0 = "zero"
digitToWord 1 = "one"
digitToWord 2 = "two"
digitToWord 3 = "three"
digitToWord 4 = "four"
digitToWord 5 = "five"
digitToWord 6 = "six"
digitToWord 7 = "seven"
digitToWord 8 = "eight"
digitToWord 9 = "nine"
