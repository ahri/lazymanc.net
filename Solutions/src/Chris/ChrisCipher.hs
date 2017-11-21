module ChrisCipher where

import Data.Char

-- data.char.ord = unicode representation of char
-- data.char.chr = char representation of unicode

shiftRight :: Int -> Char -> Char
shiftRight n c = chr $ (+) base $ mod (((ord c) - base) + n) 26 
    where 
        base = ord 'a'

shiftLeft :: Int -> Char -> Char
shiftLeft n c = chr $ (+) base $ mod (((ord c) - base) - n) 26 
    where 
        base = ord 'a'

key :: Int
key = 4

encrypt :: IO ()
encrypt = do
    s <- getLine
    let e = fmap (shiftRight key) s
    print e

decrypt :: IO ()
decrypt = do
    s <- getLine
    let e = fmap (shiftLeft key) s
    print e
