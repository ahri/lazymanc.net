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
    putStrLn "Syntax Errors"
    putStrLn $ "1. " ++ show ((++) [1, 2, 3] [4, 5, 6])  -- need brackets to prefix with an operator
    putStrLn $ "2. " ++ show ("<3" ++ " Haskell")        -- double quotes needed for Strings
    putStrLn $ "3. " ++ show (concat ["<3", " Haskell"]) -- works as-is, concat :: Foldable t => t [a] -> [a]
    putStrLn ""
    putStrLn $ "1. a. " ++ show (concat [[1, 2, 3], [4, 5, 6]]) -- works as-is
    putStrLn $ "   b. " ++ show ((++) "hello" "world") -- need brackets to prefix with an operator
    putStrLn $ "   c. " ++ show (["hello" ++ "world"]) -- missing end-quote
    putStrLn $ "   d. " ++ show ("hello" !! 4) -- index comes after the bangs
    putStrLn $ "   e. " ++ show ((!!) "hello" 4) -- works as-is


c4 :: IO ()
c4 = pure ()
