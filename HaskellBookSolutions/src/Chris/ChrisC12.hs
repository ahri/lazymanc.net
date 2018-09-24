module ChrisC12 where

c12 :: IO () 
c12 = do
    print $ "a red fox" == replaceThe "the red fox"
    print $ 10 == countVowelsF "the red fox is a very bad bad bad fox"
    print $ 10 == countVowelsS "the red fox is a very bad bad bad fox"

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
mkPersonE name age
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

replaceThe :: String -> String
replaceThe s = unwords $ fmap (\x -> if x == "the" then "a" else x) $ words s

isVowel :: Char -> Bool 
isVowel 'a' = True
isVowel 'e' = True
isVowel 'u' = True
isVowel 'i' = True
isVowel 'o' = True
isVowel _ = False

--countTheBeforeVowel :: String -> Integer
--countTheBeforeVowel s = go s b@"" c@0
--    where
--        go :: String -> String -> Integer -> Integer
--        go [] b c = c
--        go (s:ss) b c
--            | b == "" && s == 't' = go ss "t" c
--            | b == "t" && s == 'h' = go ss "th" c
--            | b == "th" && s == 'e' = go ss "the" c
--            | b == "the" && s == ' ' = go ss "the " c
--            | b == "the " && (isVowel s) = go ss "" (c+1)
--            | otherwise = go ss "" c

countVowelsF :: String -> Integer
countVowelsF s = foldr (+) 0 $ fmap (\x -> if (isVowel x) then 1 else 0) s

-- Uses sum instead of foldr
countVowelsS :: String -> Integer
countVowelsS s = sum $ fmap (\x -> if (isVowel x) then 1 else 0) s

newtype Word' = Word' String
    deriving (Eq, Show)

vowels = "aeiou"

myIterate :: (a -> a) -> a -> [a]
myIterate f a = a : myIterate f (f a)
