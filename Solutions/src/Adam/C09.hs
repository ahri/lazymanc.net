module C09 where

import           Cipher
import           Data.Char

c09 :: IO ()
c09 = do
    print $ myWords "sheryl wants fun"
    print $ myWords' "sheryl wants fun"
    putStrLn sentences
    print $ myLines sentences == shouldEqual
    print sqrCubeTuples
    print $ filter multiplesOfThree [1..30]
    print . length . filter multiplesOfThree $ [1..30]
    print . filter articless . words $ "the brown dog was a goof"
    print $ zip' [1, 2] [3, 4]
    print $ zip' [1] [3, 4]
    print $ zipWith' (+) [1, 2] [3, 4]
    print $ zip'' [1, 2] [3, 4]
    print $ zip'' [1] [3, 4]
    print $ filter isUpper "HbEfLrLxO,"
    print $ capitalize "julie"
    print $ capitalizeAll "woot"

    let encoded = caesar (+3) "Foo Bar Baz"
    print encoded
    print $ encoded == "Irr Edu Edc"
    print $ caesar (subtract 3) encoded

    print $ myOr [False, True, False]
    print $ myAny (> 6) [1, 5, 7]
    print $ myElem 6 [4, 5, 6]
    print $ myElem' 6 [4, 5, 6]
    print $ myReverse [4, 5, 6]
    print $ squish [[1, 2, 3], [4, 5, 6]]
    print $ squishMap (\x -> [x, x]) [1, 2, 3]
    print $ squishAgain [[1, 2, 3], [4, 5, 6]]
    print $ myMaximumBy compare [1, 53, 9001, 10]
    print $ myMinimumBy compare [1, 53, 9001, 10]
    print $ myMaximum [1, 53, 9001, 10]
    print $ myMinimum [1, 53, 9001, 10]

myWords :: String -> [String]
myWords ""       = []
myWords (' ':cs) = myWords cs
myWords s        = firstWord s : (myWords . afterFirstWord $ s)
    where
        firstWord :: String -> String
        firstWord = takeWhile (/= ' ')
        afterFirstWord :: String -> String
        afterFirstWord = dropWhile (/= ' ')

mySplit :: Eq a => a -> [a] -> [[a]]
mySplit _ [] = []
mySplit x xs@(x':xs')
    | x == x' = mySplit x xs'
    | otherwise = firstItem xs : (mySplit x . afterFirstItem $ xs)
    where
        firstItem = takeWhile (/= x)
        afterFirstItem = dropWhile (/= x)

myWords' :: String -> [String]
myWords' = mySplit ' '

sentences :: String
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen
    where
        firstSen = "Tyger Tyger, burning bright\n"
        secondSen = "In the forests of the night\n"
        thirdSen = "What immortal hand or eye\n"
        fourthSen = "Could frame thy fearful\
            \ symmetry?"

myLines :: String -> [String]
myLines = mySplit '\n'

shouldEqual :: [String]
shouldEqual =
    [ "Tyger Tyger, burning bright"
    , "In the forests of the night"
    , "What immortal hand or eye"
    , "Could frame thy fearful symmetry?"
    ]

sqrCubeTuples :: [(Integer, Integer)]
sqrCubeTuples = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]
    where
        mySqr = [x^2 | x <- [1..5]]
        myCube = [x^3 | x <- [1..5]]

multiplesOfThree :: Integral a => a -> Bool
multiplesOfThree n = n `mod` 3 == 0

articless :: String -> Bool
articless = (`notElem` ["the", "a", "an"])

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _          = []
zip' _ []          = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _          = []
zipWith' _ _ []          = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

zip'' :: [a] -> [b] -> [(a, b)]
zip'' = zipWith' (,)

capitalize :: String -> String
capitalize ""     = ""
capitalize (c:cs) = toUpper c : cs

capitalizeAll :: String -> String
capitalizeAll ""     = ""
capitalizeAll (c:cs) = toUpper c : capitalizeAll cs

myOr :: [Bool] -> Bool
myOr []       = False
myOr (True:_) = True
myOr (_:xs)   = myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs)
        | f x = True
        | otherwise = myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys)
        | x == y = True
        | otherwise = myElem x ys

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = myAny (== x)

myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = xs ++ [x]

squish :: [[a]] -> [a]
squish []     = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ []     = []
squishMap f (x:xs) = f x ++ squishMap f xs

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy _ [x] = x
myMaximumBy f (x:xs) = case f x recurse of
    LT -> recurse
    GT -> x
    EQ -> x
    where recurse = myMaximumBy f xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy _ [x] = x
myMinimumBy f (x:xs) = case f x recurse of
    LT -> x
    GT -> recurse
    EQ -> x
    where recurse = myMaximumBy f xs

myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare
