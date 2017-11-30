module ChrisC10 where

import Data.Time

testC10 :: IO ()
testC10 = do
    print $ foldr (*) 1 [1..5] == foldl (*) 1 [1..5]
    print $ foldr (*) 1 [1..5] == foldl (flip (*)) 1 [1..5]
    print "foldr, but not foldl, associates to the right"
    print "catamorphisms reduce structure"
    print $ filterDbDate theDatabase
    print $ filterDbString theDatabase
    print $ filterDbNumber theDatabase
 --   print $ getMostCommon theDatabase

data DatabaseItem
    = DbString String
    | DbNumber Integer
    | DbDate UTCTime
    deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase = 
    [ DbDate (UTCTime
             (fromGregorian 1911 5 1)
      (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbNumber 1145620
    , DbString "Hello, world!"
    , DbString "This sentence is false!"
    , DbDate (UTCTime
             (fromGregorian 1811 10 11)
      (secondsToDiffTime 34122))
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr f []
    where
        f (DbDate d) r = d : r
        f _ r = r

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr f []
    where
        f (DbNumber n) r = n : r
        f _ r = r

filterDbString :: [DatabaseItem] -> [String]
filterDbString = foldr f [] 
    where
        f (DbString s) r = s : r
        f _ r = r

getMostRecent :: [DatabaseItem] -> UTCTime
getMostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

--avgDb :: [DatabaseItem] -> Double
--avgDb = (div x) . sum . filterDbNumber
--    where
--        x = length filterDbNumber

fibs :: Num a => [a]
fibs = 1 : scanl (+) 1 fibs

fibs20 :: Num a => [a]
fibs20 = take 20 $ fibs

fibsF :: (Num a, Ord a) => [a]
fibsF = filter (\x -> x < 100) fibs

factorialR :: Num a => [a] -> [a]
factorialR = scanl (*) 1

factorialR20 :: Num a => [a] -> [a]
factorialR20 ns = take 20 $ factorialR ns

--1. Given the following sets of consonants and vowels:
--  stops = "pbtdkg"
--  vowels = "aeiou"
    -- Write a function that takes inputs from stops and vowels
    -- and makes 3-tuples of all possible stop-vowel-stop combinations. 

stops = "pbtkgd"
vowels = "oeiua"

--f _ [] = undefined
f ss vs = go
    where
        gp [] (v1:vs) = go ss vs
        go _ [] = []
        go (s1:s2:ss) (v1: vs) = (s1, v1, s2) : (f ss (v1:vs)) 

-- take a vowel and take two stops - repeat until all stops used up
-- drop the vowel and repeat with a new one
--f :: [(Char, Char, Char)]

-- 2. 
    -- Modify that function so that it only returns the combinations that begin with a p.

-- 3. 
    -- Now set up lists of nouns and verbs (instead of stops and
    -- vowels) and modify the function to make tuples represent-
    -- ing possible noun-verb-noun sentences
