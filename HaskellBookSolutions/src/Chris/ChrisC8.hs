module ChrisC8 where

type Answer = Bool

-- applyTimes 5 (+1) 5
-- (+1) . applyTimes (4) (+1) $ 5
-- (+1) . (+1) . applyTimes (3) (+1) 5
-- (+1) . (+1) . (+1) . applyTimes (2) (+1) 5
-- (+1) . (+1) . (+1) . (+1) . applyTimes (1) (+1) 5
-- (+1) . (+1) . (+1) . (+1) . (+1) . applyTimes (0) (+1) 5
-- (+1) . (+1) . (+1) . (+1) . (+1) . 5
-- (+1) . (+1) . (+1) . (+1) . 6
-- (+1) . (+1) . (+1) . 7
-- (+1) . (+1) . 8
-- (+1) . 9
-- 10

-- Chapter Exercises

-- 1. What is the type of [[True, False], [True, True], [False, True]]
-- [[Bool]]

-- 2. Which of the following has the same type as [[True, False], [True, True], [False, True]]
-- [[3 == 3], [6 > 5], [3 < 4]]

-- 3. For the following function
--      func :: [a] -> [a] -> [a]
--      func x y = x ++ y
-- which of the following is true?
-- all of the above

-- For the func code above, which is a valid application of func to both of its arguments?
-- func "Hello" "World"i

-- Reviewing Currying

cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appendCatty :: String -> String
appendCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

q1 :: Answer
q1 = appendCatty "woohoo!" == "woops mrow woohoo!"
--
q2 :: Answer
q2 = frappe "1" == "1 mrow haha"
q3 :: Answer
q3 = frappe (appendCatty "2") == "woops mrow 2 mrow haha"
q4 :: Answer
q4 = appendCatty (frappe "blue") == "woops mrow blue mrow haha"
q5 :: Answer
q5 = cattyConny (frappe "pink") (cattyConny "green" (appendCatty "blue")) == "pink mrow haha mrow green mrow woops mrow blue"
q6 :: Answer
q6 = cattyConny (flippy "Pugs" "are") "awesome" == "are mrow Pugs mrow awesome"

-- Recursion

--Write a function that recursively sums all numbers from 1 to n,
--n being the argument. So that if n was 5, you’d add 1 + 2 + 3 + 4
-- 5 to get 15. The type should be (Eq a, Num a) => a -> a.

sumRecursive :: (Eq a, Num a) => a -> a
sumRecursive n = go n 0
    where
        go n acc
            | n == 0 = acc
            | otherwise = go (n - 1) (acc + n)

multiplyRecSum :: (Integral a) => a -> a -> a
multiplyRecSum n m
    | n == 1 = m
    | otherwise = m + multiplyRecSum (n - 1) m

