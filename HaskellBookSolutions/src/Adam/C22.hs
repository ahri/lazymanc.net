module C22 where

import           Control.Applicative
import           Data.Char

c22 :: IO ()
c22 = do
    print $ composed "Julie"
    print $ fmapped "Chris"

cap :: String -> String
cap = map toUpper

rev :: String -> String
rev = reverse

composed :: String -> String
composed = cap . rev

fmapped :: String -> String
fmapped = cap <$> rev

newtype Reader r a =
    Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id

newtype HumanName =
    HumanName String
    deriving (Eq, Show)

newtype DogName =
    DogName String
    deriving (Eq, Show)

newtype Address =
    Address String
    deriving (Eq, Show)

data Person = Person
    { humanName :: HumanName
    , dogName   :: DogName
    , address   :: Address
    } deriving (Eq, Show)

data Dog = Dog
    { dogsName    :: DogName
    , dogsAddress :: Address
    } deriving (Eq, Show)

pers :: Person
pers =
    Person (HumanName "Big Bird")
    (DogName "Barkley")
    (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen")
    (DogName "Papu")
    (Address "Austin")

getDog :: Person -> Dog
getDog p =
    Dog (dogName p) (address p)

getDogR :: Person -> Dog
getDogR =
    Dog <$> dogName <*> address

getDogR' :: Person -> Dog
getDogR' =
    liftA2 Dog dogName address

getDog'' :: Person -> Dog
getDog'' = do
    dName <- dogName
    addr <- address
    pure $ Dog dName addr

getDog''' :: Person -> Dog
getDog''' = dogName >>= (\n -> address >>= (\a -> pure $ Dog n a))


myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f a b = f <$> a <*> b

asks :: (r -> a) -> Reader r a
asks f = Reader f


r = Reader (1+)




foo :: Int -> Int
foo = do
    double <- (2*)
    triple <- (3*)

    pure $ double + triple



data Parameters = Parameters
    { collaboratorUrl :: String
    , pollingInterval :: Int
    }

stuff :: Parameters -> String
stuff = do
    cu <- collaboratorUrl
    pi <- pollingInterval
    pure $ "output: " ++ cu ++ " " ++ (show pi)


stuff2 :: Parameters -> String
stuff2 p = "output: " ++ (collaboratorUrl p) ++ " " ++ (show . pollingInterval $ p)


stuff3 :: Parameters -> String
stuff3 p = "output: " ++ cu ++ " " ++ (show pi)
    where
        cu = collaboratorUrl p
        pi = pollingInterval p
