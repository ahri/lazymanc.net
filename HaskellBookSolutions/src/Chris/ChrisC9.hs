module ChrisC9 where

import ChrisCipher

--Using takeWhile and dropWhile, write a function that takes a string
--and returns a list of strings, using spaces to separate the elements
--of the string into words

testC9 :: IO ()
testC9 = do
    print $ myLines "here begins a poem\n" == "here begins a poem"
    print $ getNumbers "hello number 6" == "6"

--mysplit s delim = 

myLines :: String -> String
myLines = takeWhile (/= '\n')

getNumbers :: [Char] -> [Char]
getNumbers xs = [x | x <- xs, elem x ['0'..'9']]

mySqr = [x^y | x <- [1..5], y <- [2], x < 50, y < 50]
myCube = [x^3 | x <- [1..5]]

-- Will it blow up?
-- 1. [x^y | x <- [1..5], y <- [2, undefined]]  BOTTOM
-- 2. take 1 $ [x^y | x <- [1..5], y <- [2, undefined]]   1
-- 3. sum [1, undefined, 3]   BOTTOM
-- 4. length [1, 2, undefined]   3
-- 5. length $ [1, 2, 3] ++ undefined   4
-- 6. take 1 $ filter even [1, 2, 3, undefined]   2
-- 7. take 1 $ filter even [1, 3, undefined]   BOTTOM
-- 8. take 1 $ filter odd [1, 3, undefined]   1
-- 9. take 2 $ filter odd [1, 3, undefined]   1, 3
-- 10. take 3 $ filter odd [1, 3, undefined]   BOTTOM

--1. [1, 2, 3, 4, 5]   Normal Form and Weak Head Normal Form
--2. 1 : 2 : 3 : 4 : _   WHNF
--3. enumFromTo 1 10   WHNF
--4. length [1, 2, 3, 4, 5]   WHNF
--5. sum (enumFromTo 1 10)   NF
--6. ['a'..'m'] ++ ['n'..'z']   WHNF
--7. (_, 'b')   WHNF

myZip :: [a] -> [b] -> [(a, b)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = [(x, y)] ++ myZip xs ys

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f [] _ = []
myZipWith f _ [] = []
myZipWith f (x:xs) (y:ys) = [f x y] ++ myZipWith f xs ys

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) =
    if x == True 
    then True
    else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs) =
    if (f x) == True 
    then True
    else myAny f xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

