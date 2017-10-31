module C4 where

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
