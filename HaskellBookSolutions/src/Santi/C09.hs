module C09 where
import Data.Char
import Data.List

filterUpper :: String -> String
filterUpper x = filter isUpper x

firstUpperPointFree = toUpper . head

firstUpper :: String -> String
firstUpper "woot" = "WOOT"
firstUpper x = [firstUpperPointFree x] ++ tail x

rot :: Char -> (Int -> Int -> Int) -> Int -> String
rot ' ' _ _ = " "
rot x f i = do
	let abc = if isUpper x then ['A'..'Z'] else if isDigit x then ['0'..'9'] else ['a'..'z']
	[abc !! mod (f (ord x) i - ord (head abc)) (length abc)]

caesar :: String -> (Int -> Int -> Int) -> Int -> String
caesar "" f i = ""
caesar (x:xs) f i = do (rot x f i) ++ caesar xs f i

cipher :: String -> Int -> String
cipher x i = caesar x (+) i

decipher :: String -> Int -> String
decipher x i = caesar x (-) i 

myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny f [] = False
myAny f (x:xs) = f x || myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem item [] = False
myElem item (x:xs) = x == item || myElem item xs

myElemAny :: Eq a => a -> [a] -> Bool
myElemAny item (x:xs) = any (\x -> x == item) (x:xs)

myReverse :: [a] -> [a]
myReverse [] = []
myReverse list = [list !! (length list - 1)] ++ myReverse (take (length list - 1) list)

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

myFuncBy :: ((a -> a -> Ordering) -> a -> a -> a) -> (a -> a -> Ordering) -> [a] -> a
myFuncBy minmax f (x:[]) = x
myFuncBy minmax f (x:y:[]) = minmax f x y
myFuncBy minmax f (x:y:xs) = do
	let first = minmax f x y
	minmax f first (myFuncBy minmax f xs)
	
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a	-- breaks if [a] == []
myMaximumBy f list = myFuncBy myMax f list

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a	-- breaks if [a] == []
myMinimumBy f list = myFuncBy myMin f list

myMax f x y = case (f x y) of
	GT -> x
	LT -> y
	EQ -> x
	
myMin f x y = case (f x y) of
	GT -> y
	LT -> x
	EQ -> x