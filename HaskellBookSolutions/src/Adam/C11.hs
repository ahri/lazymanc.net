{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module C11 where

import           Cipher

c11 :: IO ()
c11 = do
    print $ vigenereEncrypt "ALLY" "MEET AT DAWN"
    print $ vigenereEncrypt "ALLY" "MEET AT DAWN" == "MPPR AE OYWY"
    print $ vigenereDecrypt "ALLY" "MPPR AE OYWY" == "MEET AT DAWN"

    print $ eval (Add (Lit 1) (Lit 9001))

    let a1 = Add (Lit 9001) (Lit 1)
    let a2 = Add a1 (Lit 20001)
    let a3 = Add (Lit 1) a2
    print $ printExpr a3


data Price = Price Integer deriving (Eq, Show)
data Size = Small | Medium | Large Integer deriving (Eq, Show)
data Manufacturer = Mini | Mazda | Tata deriving (Eq, Show)
data Airline = PapuAir | CatapultsR'Us | TakeYourChancesUnited deriving (Eq, Show)
data Vehicle = Car Manufacturer Price | Plane Airline Size Price

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir Medium

isCar :: Vehicle -> Bool
isCar (Car _ _) = True
isCar _         = False

isPlane :: Vehicle -> Bool
isPlane (Plane _ _ _) = True
isPlane _             = False

areCars :: [Vehicle] -> [Bool]
areCars = fmap isCar

getManu :: Vehicle -> Manufacturer
getManu (Car m _) = m

class TooMany a where
    tooMany :: a -> Bool

instance TooMany Int where
    tooMany n = n > 42

newtype Goats = Goat Int deriving (Eq, Show, TooMany)

newtype Foo = Foo (Int, String) deriving (Eq, Show)

data Expr
    = Lit Integer
    | Add Expr Expr

eval :: Expr -> Integer
eval (Lit i)   = i
eval (Add x y) = eval x + eval y

printExpr :: Expr -> String
printExpr (Lit i)   = show i
printExpr (Add x y) = printExpr x ++ " + " ++ printExpr y
