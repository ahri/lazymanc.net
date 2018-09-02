module TestX where
import GHC.Int
import Data.Tuple

data Side = Heads | Tails deriving (Show, Eq)
flipCoin :: Side -> Side
flipCoin Heads = Tails
flipCoin _ = Heads

flipCoinIf :: Side -> Side
flipCoinIf x = do
	if x == Heads then Tails else Heads

rvrs :: String -> String
rvrs x = do 	-- Curry is awesome
	a ++ " " ++ b ++ " " ++ c where
		a = drop 9 x
		b = take 2 $ drop 6 x
		c = take 5 x	
		
charAt :: String -> Int -> Char
charAt x i = do	
	x !! i
	
chp4 = do
	let awesome = ["a", "b", "c"]
	let	also = ["d", "e"]
	let allAwesome = concat [awesome, also]
	allAwesome
	
palindrome :: String -> Bool
palindrome x = do
	x == reverse x
	
getAbs :: Integer -> Integer
getAbs x = do
	if x < 0 then x * (-1) else x
	
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y = do
	((snd x, snd y), (fst x, fst y))
	
g :: Int -> Bool
g 1 = True

addOneIfOdd n = case odd n of
	True -> f n
	False -> n
	where f = \n -> n + 1

addFive = \x -> \y -> (if x > y then y else x) + 5

z :: (((a -> b) -> c) -> d) -> a
z = undefined

max' x y = case x > y of
	True -> x 
	False -> y
	
--ifEvenAddTwo n = if even n then (n + 2) else n
ifEvenAddTwo n = case even n of 
	True -> n + 2
	False -> n
	
sign x = case compare x 0 of
	LT -> -1
	GT -> 1
	EQ -> 0
	
main :: IO()
main = do
	putStrLn $ "plus " ++ "concat"
	putStrLn $ concat ["concat", " function"]
	putStrLn $ rvrs "Curry is awesome"
	print $ flipCoin Heads
	print $ flipCoinIf Heads
	print $ palindrome "abc"
	print $ palindrome "aba"
	print $ getAbs (-10)
	putStrLn $ show $ getAbs 10
	print $ f (1, 2) (3, 4)