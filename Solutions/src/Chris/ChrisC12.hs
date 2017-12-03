module ChrisC12 where

C12 :: IO () 
C12 = do
    print $ "testing C12"

type Name = String
type Age = Integer

data Person = Person Name Age 
    deriving (Show)

-- With Maybe

mkPersonM :: Name -> Age -> Maybe Person
mkPersonM name age
    | name /= "" && age > 0 = Just $ Person name age
    | otherwise = Nothing

-- With Either

-- Represent invalid data
data PersonInvalid = NameEmpty | AgeTooLow
    deriving (Eq, Show)

-- data Either a b = Left a | Right b

mkPersonE :: Name -> Age -> Either PersonInvalid Person
mkPersonE
    | name /= "" && age > 0 = Right $ Person name age
    | name == "" = Left NameEmpty
    | age < 0 = Left AgeTooLow

--1. Given
--    id :: a -> a
--    What is the kind of a?
-- * -> *

--2. r :: a -> f a
--    What are the kinds of a and f?
-- a: *
-- f: * (because it is applied)



