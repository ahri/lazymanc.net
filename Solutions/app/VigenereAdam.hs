module Main where

import           Cipher
import           System.Environment
import           System.Exit
import           System.IO

main :: IO ()
main = do
    args <- getArgs

    case args of
        [key, "-e"] -> getContents >>= (putStrLn . vigenereEncrypt key)
        [key, "-d"] -> getContents' >>= (putStrLn . vigenereDecrypt key)
        _           -> hPutStrLn stderr "nope" >> exitFailure

getContents' :: IO String
getContents' = do
    atEnd <- hIsEOF stdin
    if atEnd
    then pure ""
    else do
        c <- hGetChar stdin
        ([c] ++) <$> getContents'
