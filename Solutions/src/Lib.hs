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

c3 :: IO ()
c3 = pure ()

waxOn :: Integer
waxOn = x * 5
    where
        x = y ^ 2
        y = z + 8
        z = 7

waxOff :: Integer -> Integer
waxOff x = flip (^) 2 . triple $ x

triple :: Integer -> Integer
triple x = x * 3
