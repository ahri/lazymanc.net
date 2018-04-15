module Lib where

import           C02
import           C03
import           C04
import           C05
import           C06
import           C07
import           C08
import           C09
import           C10 hiding (myAnd)
import           C11
import           C12
import           C16

chapters :: IO ()
chapters = output [c02, c03, c04, c05, c06, c07, c08, c09, c10, c11, c12, pure (), pure (), pure (), c16, pure (), pure (), pure (), pure (), pure (), c22] 2
    -- print $ foldl (\(x:xs, _) c -> (xs, c + x)) ([3, 2], 0) [4, 5]
    -- print $ myFoldl (\(x:xs, _) c -> (xs, c + x)) ([3, 2], 0) [4, 5]
    -- print $ foldr (\c (x:xs, _) -> (xs, c + x)) ([3, 2], 0) [4, 5]
    -- print $ myFoldr (\c (x:xs, _) -> (xs, c + x)) ([3, 2], 0) [4, 5]
    -- print $ myFoldr myAnd False $ repeat False
    -- print $ False `myAnd` undefined

output :: [IO ()] -> Integer -> IO ()
output [] _     = pure ()
output (f:fs) i = do
    putStrLn ""
    putStrLn ""
    putStrLn $ "Chapter " ++ show i ++ "."
    putStrLn ""
    f
    output fs (i + 1)

myRev :: [a] -> [a]
myRev []     = []
myRev (x:xs) = myRev xs ++ [x]

myFoldl :: (acc -> curr -> acc) -> acc -> [curr] -> acc
myFoldl _ acc []        = acc
myFoldl f acc (curr:xs) = myFoldl f (f acc curr) xs

myFoldr :: (curr -> acc -> acc) -> acc -> [curr] -> acc
myFoldr _ acc []        = acc
myFoldr f acc (curr:xs) = f curr (myFoldr f acc xs)
-- if pattern matching means that "f curr undefined" returns, we hit the base case
-- why the reversal? because the acc is being passed down completely unscathed until the function base case hits

myAnd :: Bool -> Bool -> Bool
myAnd True True = True
myAnd _ _       = False
