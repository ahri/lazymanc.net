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
    print $ caesar (+3) "Foo Bar Baz"
    print $ caesar (+3) "Foo Bar Baz" == "Irr Edu Edc"

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
