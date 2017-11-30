{-# LANGUAGE TypeOperators #-}

module Cipher (caesar, vigenere) where

import           Data.Char

caesar :: (Int -> Int) -> String -> String
caesar _ ""     = ""
caesar f (c:cs) = rotate c f : caesar f cs

vigenere :: String -> String -> String
vigenere keyword = snd . foldr f (mask, "")
    where
        f c (ms'@(m:ms), str) = if isAsciiLetter c
            then (ms, c >>> shift : str)
            else (ms', c : str)

-- TODO: why isn't the (broken) algorithm above the same as this (working) one?
-- vigenere keyword str = go mask str
--     where
--         go _ "" = ""
--         go ms'@(m:ms) (c:cs) = if isAsciiLetter c
--             then c >>> shift : go ms cs
--             else c : go ms' cs

            where
                shift = if isLower c
                    then lowerShift
                    else upperShift

                lowerShift = toLower m <?> 'a'
                upperShift = toUpper m <?> 'A'

        mask = concat . repeat $ keyword

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

infixl 6 <?>
(<?>) = diff
