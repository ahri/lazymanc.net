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

--getMostCommon :: [DatabaseItem] -> UTCTime
--getMostCommon dbi = 
            
