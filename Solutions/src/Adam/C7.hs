module C7 where

c7 :: IO ()
c7 = do
    print $ addOneIfOdd 3
    print (roundTrip3 4::Int)

addOneIfOdd n = case odd n of
    True  -> f2 n
    False -> n
    where f n = n + 1
          f2 = \n -> n + 1

addFive x y = (if x > y then y else x) + 5
addFive2 = \x -> \y -> (if x > y then y else x) + 5

mflip f = \x -> \y -> f y x
mflip2 f x y = f y x

f :: (a, b, c)
  -> (d, e, f)
  -> ((a, d), (c, f))
f (a, _, c) (d, _, f) = ((a, d), (c, f))

functionC x y = if (x > y) then x else y
functionC2 x y = case x > y of
    True  -> x
    False -> y

ifEvenAdd2 n = if even n then (n+2) else n
ifEvenAdd22 n = case even n of
    True  -> n + 2
    False -> n

nums x =
    case compare x 0 of
        LT -> -1
        GT -> 1
        _  -> 0

-- 1.
tensDigit :: Integral a => a -> a
tensDigit x = d
        where
            d = xLast `mod` 10
            xLast = x `div` 10

-- 1a.
tensDigit2 :: Integral a => a -> a
tensDigit2 = fst . (`divMod` 10)

-- 1c.
hunsD = fst . (`divMod` 100)

-- 2.
foldBool :: a -> a -> Bool -> a
foldBool x y b = case b of
    True  -> y
    False -> x

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y b
    | b = y
    | otherwise = x

foldBool3 :: a -> a -> Bool -> a
foldBool3 x _ False = x
foldBool3 _ y True  = y

-- 3.
g :: (a -> b) -> (a, c) -> (b, c)
g fab (a, c) = (fab a, c)

-- 4.
roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

-- 5.
roundTrip2 :: (Show a, Read a) => a -> a
roundTrip2 = read . show

-- 6.
roundTrip3 :: (Show a, Read b) => a -> b
roundTrip3 = read . show
