module Main where

import           Cipher
import           System.Environment
import           System.Exit
import           System.IO

main :: IO ()
main = do
    args <- getArgs

    case args of
        [key, "-e"] -> readFromStdin >>= (putStrLn . vigenereEncrypt key)
        [key, "-d"] -> readFromStdin >>= (putStrLn . vigenereDecrypt key)
        _           -> hPutStrLn stderr "nope" >> exitFailure

readFromStdin :: IO String
readFromStdin = do
    atEnd <- hIsEOF stdin
    if atEnd
        then return ""
        else do
            c <- hGetChar stdin
            ([c] ++) <$> readFromStdin
