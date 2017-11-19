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
