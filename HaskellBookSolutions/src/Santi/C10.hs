module C10 where

stops = "pbtdkg"
vowels = "aeiou"

combinations = [(first, second, third) | first <- stops, second <- vowels, third <- stops]
pCombinations = [(first, second, third) | first <- stops, second <- vowels, third <- stops, first == 'p']

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\a b -> if f a then True else b) False

myElemFold :: Eq a => a -> [a] -> Bool
myElemFold item = foldr (\a b -> if a == item then True else b) False

myElemAny :: Eq a => a -> [a] -> Bool
myElemAny item = myAny (\a -> if a == item then True else False)

myReverse :: [a] -> [a]
myReverse = foldl (\a b -> (b:a)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\a b -> (f a:b)) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter filter = foldr (\a b -> if filter a == True then (a:b) else b) []

squish :: [[a]] -> [a]
squish = foldr (\a b -> concat [a,b]) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap map = foldr (\a b -> concat [map a,b]) []