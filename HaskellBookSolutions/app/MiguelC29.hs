module Main where

import System.Environment(getArgs)
import System.Exit(exitFailure, exitSuccess)
import System.IO(hGetChar, hPutStr, hPutStrLn, hWaitForInput, stderr, stdout, stdin)
import Data.List (cycle)
import Data.Char (isDigit, isLetter)
import MiguelVigenere

usage :: IO ()
usage = putStrLn "Usage: [-d|-e] <key> [-t <timeout>]"
    
type Key = String

data Action = Encrypt | Decrypt

type Timeout = Int

data Mode = Mode Action Key (Maybe Timeout)

findOpt :: String -> [String] -> Maybe String
findOpt key args =
    case args of
        []           -> Nothing
        [_]          -> Nothing
        (s1:s2:tail) -> 
            if s1 == key then Just s2 else findOpt key (s2:tail)
        
findEncryptOpt :: [String] -> Maybe Key
findEncryptOpt = findOpt "-e"

findDecryptOpt :: [String] -> Maybe Key
findDecryptOpt = findOpt "-d"

findTimeoutOpt :: [String] -> Maybe String
findTimeoutOpt = findOpt "-t"

parseTimeout :: String -> Either String Int
parseTimeout str | all isDigit str = Right (read str)
parseTimeout str                   = Left ("'" ++ str ++ "' is not a valid timeout")

parseArgs :: [String] -> IO Mode
parseArgs args =
    case (findEncryptOpt args, findDecryptOpt args, findTimeoutOpt args) of
        (Just key, Nothing , maybeTimeoutStr) -> do
            maybeTimeout <- traverse readTimeout maybeTimeoutStr
            return $ Mode Encrypt key maybeTimeout
        (Nothing , Just key, maybeTimeoutStr) -> do
            maybeTimeout <- traverse readTimeout maybeTimeoutStr
            return $ Mode Decrypt key maybeTimeout
        _                                     -> do
            hPutStrLn stderr "Must use one of -e or -d"
            usage
            exitFailure
    where readTimeout timeoutStr = 
            either (\error -> hPutStrLn stderr error >> exitFailure)
                   return
                   (parseTimeout timeoutStr)

{-- THIS DOESN'T WORK --}

-- execute :: Mode -> String -> String
-- execute (Mode action key) =
--     case action of
--         Encrypt -> cipher key
--         Decrypt -> decipher key

-- loop :: Mode -> IO ()
-- loop mode = do
--     putStrLn "I hang here 1"
--     inputStream <- sequence (repeat $ hGetChar stdin)
--     putStrLn "I hang here 2"
--     putStrLn (execute mode inputStream)

execute :: Action -> Char -> Char -> Char
execute Encrypt = cipherChar
execute Decrypt = decipherChar

waitForInput :: Int -> IO ()
waitForInput timeout = do
    theresInput <- hWaitForInput stdin timeout
    if theresInput then
        return ()
    else do
        hPutStrLn stderr ("Didn't receive input within " ++ show timeout ++ "ms.")
        exitFailure

loop :: Action -> Key -> Maybe Timeout -> IO ()
loop action (key:keyTail) maybeTimeout = do
    maybe (return ()) waitForInput maybeTimeout
    c <- hGetChar stdin
    if not (isLetter c) then
        exitSuccess
    else do
        hPutStr stdout [execute action key c]
        loop action keyTail maybeTimeout

-- stack exec MiguelC29 -- -t 5000 -d LEMON
main :: IO ()
main = do
    Mode action key maybeTimeout <-getArgs >>= parseArgs
    loop action (cycle key) maybeTimeout