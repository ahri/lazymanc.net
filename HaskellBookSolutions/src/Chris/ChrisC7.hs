module ChrisC7 where

c7 :: IO ()
c7 = do
    return ()

-- Equivalent Expressions
mTH1 x y z = x * y * z
mTH2 x y = \z -> x * y * z
mTH3 x = \y -> \z -> x * y * z
mTH4 = \x -> \y -> \z -> x * y * z
-- They are all equivalent

-- mTH 3 :: Num a => a -> a -> a

addOneIFOdd n = case odd n of
    True -> f n
    False -> n
    where f = \n -> n + 1

addFive x y = (if x > y then y else x) + 5
addFiveLambda = \x -> \y -> (if x > y then y else x) + 5

mflip f = \x -> \y -> f y x
mflipNoL f x y = f y x

-- newtypes and pattern matching

newtype Username = Username String

newtype AccountNumber = AccountNumber Integer

data User = 
    UnregisteredUser
    | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser = putStrLn "UnregisteredUser"
printUser (RegisteredUser (Username name) (AccountNumber accNum)) = 
    putStrLn $ name ++ " " ++ show accNum

-- Questions 2
-- What is the type of k? 
-- What is the type of k2? Is it the same type as k1 or k3?
-- Of k1, k2, k3, which will return the number 3 as the result?
--k :: (a, b) -> a
--k (x, y) = x
--k1 :: (Num a, Num b) => (a, b) -> a
--k1 = k ((4-1), 10)
--k2 :: () -> (a, b) -> a
--k2 = k ("three", (1 + 2))
--k3 :: () -> (a, b) -> a
--k3 = k (3, True)
--willReturn3 = k1 && k3

f :: (a, b, c)
    -> (d, e, f)
    -> ((a, d), (c, f))
f (x, _, x') (y, _, y') = ((x, y), (x', y'))

-- Case Practice

functionC x y = if (x > y) then x else y
functionC' x y = case x > y of
    True -> x
    False -> y

ifEvenAdd2 n = if even n then (n+2) else n
ifEvenAdd2' n = case even n of
    True -> n + 2
    False -> n

nums x =
    case compare x 0 of
        LT -> -1
        EQ -> 0
        GT -> 1

-- Artful Dodgy
dodgy :: (Num a) => a -> a -> a
dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2

--2. dodgy 1 1          11
--3. dodgy 2 2          22
--4. dodgy 1 2          21
--5. dodgy 2 1          12
--6. oneIsOne 1         11
--7. oneIsOne 2         21
--8. oneIsTwo 1         20
--9. oneIsTwo 2         22
--10. oneIsOne 3        31
--11. oneIsTwo 3        23

enumFrom3_8 = take 5 . enumFrom $ 3

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTripPF :: (Show a, Read a) => a -> a
roundTripPF = read . show
