module C09 where

c09 :: IO ()
c09 = do
    print $ myWords "sheryl wants fun"
    print $ myWords' "sheryl wants fun"
    putStrLn sentences
    print $ myLines sentences == shouldEqual

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

sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen
    where
        firstSen = "Tyger Tyger, burning bright\n"
        secondSen = "In the forests of the night\n"
        thirdSen = "What immortal hand or eye\n"
        fourthSen = "Could frame thy fearful\
            \ symmetry?"

myLines = mySplit '\n'

shouldEqual =
    [ "Tyger Tyger, burning bright"
    , "In the forests of the night"
    , "What immortal hand or eye"
    , "Could frame thy fearful symmetry?"
    ]
