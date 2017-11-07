module C03 where

c03 :: IO ()
c03 = do
    putStrLn   "Syntax errors"
    putStrLn $ "1.    " ++ show ((++) [1, 2, 3] [4, 5, 6])  -- need brackets to prefix with an operator
    putStrLn $ "2.    " ++ show ("<3" ++ " Haskell")        -- double quotes needed for Strings
    putStrLn $ "3.    " ++ show (concat ["<3", " Haskell"]) -- works as-is, concat :: Foldable t => t [a] -> [a]
    putStrLn   ""
    putStrLn   "Exercises"
    putStrLn   "Reading syntax"
    putStrLn $ "1. a. " ++ show (concat [[1, 2, 3], [4, 5, 6]]) -- works as-is
    putStrLn $ "   b. " ++ show ((++) [1, 2, 3] [4, 5, 6]) -- works as-is
    putStrLn $ "   c. " ++ show ((++) "hello" "world") -- need brackets to prefix with an operator
    putStrLn $ "   d. " ++ show (["hello" ++ "world"]) -- missing end-quote
    putStrLn $ "   e. " ++ show ("hello" !! 4) -- index comes after the bangs
    putStrLn $ "   f. " ++ show ((!!) "hello" 4) -- works as-is
    putStrLn $ "   g. " ++ show (take 4 "lovely") -- first param in wrong place
    putStrLn $ "   h. " ++ show (take 3 "awesome") -- fine as-is
    putStrLn   ""
    putStrLn   "2. a. d"
    putStrLn   "   b. c"
    putStrLn   "   c. e"
    putStrLn   "   d. a"
    putStrLn   "   e. b"
    putStrLn   ""
    putStrLn   "Building functions"
    putStrLn $ "1. a. " ++ bf_1a input
    putStrLn $ "   b. " ++ (bf_1b . bf_1a $ input)
    putStrLn $ "   c. " ++ (bf_1c . bf_1a $ input)
    putStrLn   "2.    as (1)"
    putStrLn $ "3.    " ++ show (thirdLetter input)
    putStrLn $ "4.    " ++ show (letterIndex 3)
    putStrLn $ "5.    " ++ show (rvrs input)
    putStrLn   "6.    not bothering to modularize"

    where
        input = "Curry is awesome"
        bf_1a = (++"!")
        bf_1b = (:"") . (!!4)
        bf_1c = drop 9

        thirdLetter :: String -> Char
        thirdLetter = (!!3)

        letterIndex :: Int -> Char
        letterIndex x = input !! x

        rvrs :: String -> String
        rvrs cs = slice 9 7 cs ++ slice 5 4 cs ++ slice 0 5 cs
            where slice idx size = take size . drop idx
