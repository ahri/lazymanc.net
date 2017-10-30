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


data Mood = Blah | Woot deriving Show

c4 :: IO ()
c4 = do
    putStrLn   "4.3 Mood Swing"
    putStrLn   "1. Mood"
    putStrLn   "2. Blah, Woot"
    putStrLn   "3. should be Mood -> Mood"
    putStrLn   "4. should be pattern matching on Blah (value), not Mood (type)"
    putStrLn $ "5. " ++ (show . changeMood $ Blah) ++ " " ++ (show . changeMood $ Woot)
    putStrLn   ""
    putStrLn   "4.6 Exercises: Find the Mistakes"
    putStrLn $ "1. " ++ show (not True && True) -- value True is capitalised (it's a type constructor)
    putStrLn $ "2. " ++ show (not (x == 6)) -- comparison with ==, not =
    putStrLn $ "3. " ++ show ((1 * 2) > 5) -- works as-is
    putStrLn $ "4. " ++ show ("Merry" > "Happy") -- needs to be in lists
    putStrLn $ "5. " ++ show (['1', '2', '3'] ++ "look at me!") -- only same types can be concatenated
    putStrLn   ""
    putStrLn   "4.9 Chapter Exercises"
    putStrLn   " 1. [a] -> Int" -- meh, Foldable
    putStrLn $ " 2. a. 5 vs. " ++ show (length [1, 2, 3, 4, 5])
    putStrLn $ "    b. 3 vs. " ++ show (length [(1, 2), (2, 3), (3, 4)])
    putStrLn $ "    c. 2 vs. " ++ show (length allAwesome)
    putStrLn $ "    d. 5 vs. " ++ show (length . concat $ allAwesome)
    putStrLn $ " 4. " ++ show (6 `div` length [1, 2, 3])
    putStrLn $ " 5. Boolean, True vs. " ++ show (2 + 3 == 5)
    putStrLn $ " 6. Boolean, False vs. " ++ show (let x = 6 in x + 3 == 5)
    putStrLn $ " 7. a. yes, -> True vs. " ++ show (length allAwesome == 2)
    putStrLn   "    b. no"
    putStrLn $ "    c. yes, -> 5 vs. " ++ show (length allAwesome + length awesome)
    putStrLn $ "    d. yes, -> False vs. " ++ show ((8 == 8) && ('b' < 'a'))
    putStrLn   "    e. no"
    putStrLn $ " 8. palindrome: blahhalb -> " ++ show (isPalindrome "blahhalb")
    putStrLn $ " 9. myAbs 9 = " ++ show (myAbs 9) ++ ", myAbs -9 = " ++ show (myAbs (-9))
    putStrLn $ "10. f ('a', 'b') ('c', 'd') = " ++ show (f ('a', 'b') ('c', 'd'))
    putStrLn   ""
    putStrLn   "Match the function names to their types"
    putStrLn   "1. show: c) Show a => a -> String"
    putStrLn   "2. (==): b) Eq a => a -> a -> Bool"
    putStrLn   "3. fst:  a) (a, b) -> a"
    putStrLn   "4. (+):  d) Num a => a -> a -> a"

    where
        changeMood Blah = Woot
        changeMood _    = Blah
        x = 6
        awesome = ["Papuchon", "curry", ":)"]
        also = ["Quake", "The Simons"]
        allAwesome = [awesome, also]

        isPalindrome :: (Eq a) => [a] -> Bool
        isPalindrome x = x == reverse x

        myAbs :: Integer -> Integer
        myAbs n = if n < 0
            then -n
            else  n

        f :: (a, b) -> (c, d) -> ((b, d), (a, c))
        f t1 t2 = ((snd t1, snd t2), (fst t1, fst t2))

        c4_cs_1 xs = w `x` 1
            where
                x = (+)
                w = length xs

        c4_cs_2 x = x

        c4_cs_3 (a, b) = a
