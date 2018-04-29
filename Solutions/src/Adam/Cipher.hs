{-# LANGUAGE TypeOperators #-}

module Cipher (caesar, vigenereEncrypt, vigenereDecrypt) where

import           Data.Char
import           Data.List

caesar :: (Int -> Int) -> String -> String
caesar _ ""     = ""
caesar f (c:cs) = rotate c f : caesar f cs

vigenereEncrypt :: String -> String -> String
vigenereEncrypt = vigenere (>>>)

vigenereDecrypt :: String -> String -> String
vigenereDecrypt = vigenere (<<<)

vigenere :: (Char -> Int -> Char) -> String -> String -> String
vigenere rotator keyword input = snd . foldl' f (mask, "") $ input
    where
        f (ms'@(m:ms), str) c = if isAsciiLetter c
            then (ms, str ++ [rotator c shift])
            else (ms', str ++ [c])

            where
                shift = if isLower c
                    then lowerShift
                    else upperShift

                lowerShift = diff (toLower m) 'a'
                upperShift = diff (toUpper m) 'A'

        mask = take (length input) . concat . repeat $ keyword

diff :: Char -> Char -> Int
diff x y = ord x - ord y

rotate :: Char -> (Int -> Int) -> Char
rotate c f
    | c `elem` ['a'..'z'] = conv 'a' c
    | c `elem` ['A'..'Z'] = conv 'A' c
    | otherwise = c
    where
        conv base char = chr . (+ baseN) . (`mod` 26) . f $ ord char - baseN
            where baseN = ord base

isAsciiLetter :: Char -> Bool
isAsciiLetter = (`elem` (['a' .. 'z'] ++ ['A' .. 'Z']))

infixl 6 >>>
(>>>) :: Char -> Int -> Char
c >>> i = rotate c (+ i)

infixl 6 <<<
(<<<) :: Char -> Int -> Char
c <<< i = rotate c (subtract i)

infixl 6 <?>
(<?>) = diff
