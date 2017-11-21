module C10 where

import           Data.Time

c10 :: IO ()
c10 = do
    print $ foldr const 0 [1..]
    print $ foldr (++) "" ["woot", "WOOT", "woot"]
    print $ foldl e5epf3 "" [1..5]

    print $ filterDbDate theDatabase
    print $ filterDbNumber theDatabase
    print $ mostRecent theDatabase
    print $ sumDb theDatabase
    print $ avgDb theDatabase

    print $ myAny even [1, 3, 5]
    print $ myAny odd [1, 3, 5]

    print $ myAny' even [1, 3, 5]
    print $ myAny' odd [1, 3, 5]

    print $ myElem 1 [1..10]
    print $ myElem 1 [2..10]

    print $ myReverse [1..5]

    print $ myMap (+ 1) [1..3]

    print $ myFilter even [1..3]

    print $ squish [[1, 2, 3], [4, 5, 6]]
    print $ squishMap id [[1, 2, 3], [4, 5, 6]]
    print $ squishAgain [[1, 2, 3], [4, 5, 6]]

    where
        e5e :: String -> Int -> String
        e5e s n = s ++ show n
        e5epf1 s n = (++) s (show n)
        e5epf2 s = (. show) . (++) $ s
        e5epf3 = (. show) . (++)

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
    [ DbDate (UTCTime
               (fromGregorian 1911 5 1)
               (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbNumber 6
    , DbString "Hello World!"
    , DbDate (UTCTime
               (fromGregorian 1921 5 1)
               (secondsToDiffTime 34123))
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate = foldr f []
    where
        f (DbDate t) r = t : r
        f _ r          = r

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr f []
    where
        f (DbNumber n) r = n : r
        f _ r            = r

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb db = sum db / count db
        where
            sum = fromIntegral . sumDb
            count = fromIntegral . length . filterDbNumber

stops = "pbtdkg"
vowels = "aeiou"

combinations = [(s1, v, s2) | s1 <- stops, v <- vowels, s2 <- stops, s1 == 'p']

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr f''' False
    where
        f' c a = a || f c
        f'' c a = (||) a $ f c
        f''' = ((||) .) f

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' = (`foldr` False) . ((||) .)

myElem :: Eq a => a -> [a] -> Bool
myElem x = (`foldr` False) (((||) .) (x ==)) -- TODO: pf

myReverse :: [a] -> [a]
myReverse = foldr f []
    where f c a = a ++ [c]

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr f' []
    where f' c a = f c : a

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f = foldr f' []
    where
        f' c a =
            if f c
            then c : a
            else a

squish :: [[a]] -> [a]
squish = foldr f []
    where f c a = c ++ a

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = foldr f' []
    where f' c a = f c ++ a

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

