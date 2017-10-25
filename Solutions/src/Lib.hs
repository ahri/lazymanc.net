module Lib where

c2 :: IO ()
c2 = do
    putStrLn "Parenthesization:"
    putStrLn $ "1. " ++ show (2 + 2 * 3 - 1)     ++ "    vs " ++ show ((2 + (2 * 3)) - 1)
    putStrLn $ "2. " ++ show ((^) 10 $ 1 + 1)    ++ "  vs " ++ show (10^(1 + 1))
    putStrLn $ "3. " ++ show (2 ^ 2 * 4 ^ 5 + 1) ++ " vs " ++ show (((2^2) * (4^5)) + 1)
    putStrLn ""
    putStrLn "Equivalent expressions"
    putStrLn $ "1. 1 + 1       == 2"            ++ "?            -> " ++ show (1 + 1                      == 2)
    putStrLn $ "2. 10 ^ 2      == 10 + 9 * 10"  ++ "?  -> "           ++ show (10^2                       == 10 + 9*10)
    putStrLn $ "3. 400 - 37    == (-) 37 400"   ++ "?   -> "          ++ show (400 - 37                   == (-) 37 400)
    putStrLn $ "4. 100 `div` 3 == 100 / 3"      ++ "?      -> "       ++ show (fromIntegral (100 `div` 3) == 100 / 3)
    putStrLn $ "5. 2 * 5 + 18  == 2 * (5 + 18)" ++ "? -> "            ++ show (2 * 5 + 18                 == 2 * (5 + 18))

waxOn :: Integer
waxOn = x * 5
    where
        x = y ^ 2
        y = z + 8
        z = 7

waxOff :: Integer -> Integer
waxOff = (^ 2) . triple

triple :: Integer -> Integer
triple x = x * 3

c3 :: IO ()
c3 = do
    putStrLn   "Syntax errors"
    putStrLn $ "1.    " ++ show ((++) [1, 2, 3] [4, 5, 6])  -- need brackets to prefix with an operator
    putStrLn $ "2.    " ++ show ("<3" ++ " Haskell")        -- double quotes needed for Strings
    putStrLn $ "3.    " ++ show (concat ["<3", " Haskell"]) -- works as-is, concat :: Foldable t => t [a] -> [a]
    putStrLn   ""
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
        bf_1b = pure . (!!4)
        bf_1c = drop 9

        thirdLetter :: String -> Char
        thirdLetter = (!!3)

        letterIndex :: Int -> Char
        letterIndex x = input !! x

        rvrs :: String -> String
        rvrs cs = slice 9 7 cs ++ slice 5 4 cs ++ slice 0 5 cs
            where slice idx size = take size . drop idx


c4 :: IO ()
c4 = pure ()
