module MiguelVigenere where

import Data.Char (ord, chr, isUpper, isLower)
import Data.List (cycle)

base :: Char -> Int
base c | isLower c = ord 'a'
base c | isUpper c = ord 'A'

index :: Char -> Int
index c = ord c - base c

shiftBy :: (Int -> Int -> Int) -> Char -> Char -> Char
shiftBy op shift c = chr $ (index c `op` index shift) `mod` 26 + base c

cipherChar :: Char -> Char -> Char
cipherChar = shiftBy (+)

cipher :: String -> String -> String
cipher key text = zipWith cipherChar (cycle key) text

decipherChar :: Char -> Char -> Char
decipherChar = shiftBy (-)

decipher :: String -> String -> String
decipher key text = zipWith decipherChar (cycle key) text