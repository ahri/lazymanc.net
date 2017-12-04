module Day01 where

t1 = [1, 1, 2, 2] :: [Int]

t2 = [1, 1, 1, 1] :: [Int]

t3 = [1, 2, 3, 4] :: [Int]

t4 = [9, 1, 2, 1, 2, 1, 2, 9] :: [Int]

captcha :: [Int] -> Int
captcha l@(fl:_) = go l 0
    where
        go [] s       = s
        go [x] s      = sumOnMatch s x fl
        go (x:y:xs) s = go (y:xs) $ sumOnMatch s x y

        sumOnMatch s x y
            | x == y = s + x
            | otherwise = s
