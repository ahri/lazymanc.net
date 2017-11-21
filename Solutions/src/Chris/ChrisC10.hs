module ChrisC10 where

testC10 :: IO ()
testC10 = do
    print $ foldr (*) 1 [1..5] == foldl (*) 1 [1..5]
    print $ foldr (*) 1 [1..5] == foldl (flip (*)) 1 [1..5]
    print "foldr, but not foldl, associates to the right"
    print "catamorphisms reduce structure"

-- foldr (*) 1 [1..5] -- 120
--      will return the same result as which of the following:
-- a) flip (*) 1 [1..5]
-- b) foldl (flip (*)) 1 [1..5] - 120
-- c) foldl (*) 1 [1..5] -- 120
