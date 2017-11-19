module Cipher where

import           Data.Char

caesar :: (Int -> Int) -> String -> String
caesar _ "" = ""
caesar f (c:cs) = replacement : caesar f cs
    where
        replacement
            | c `elem` ['a'..'z'] = conv 'a' c
            | c `elem` ['A'..'Z'] = conv 'A' c
            | otherwise = c

        conv base char = chr . (+ baseN) . (`mod` 26) . f $ ord char - baseN
            where baseN = ord base
