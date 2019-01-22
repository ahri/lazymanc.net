#!/usr/bin/env stack
{- stack --resolver lts-12.9 script
    --package QuickCheck
-}

{- COMPILE_FLAGS -O2 -threaded -rtsopts -eventlog -}

-- https://downloads.haskell.org/~ghc/8.2.2/docs/html/users_guide/using-warnings.html
{-# OPTIONS_GHC -Werror -Wall -Wcompat                                  #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns -Wincomplete-record-updates   #-}
{-# OPTIONS_GHC -Widentities -Wredundant-constraints                    #-}
{-# OPTIONS_GHC -Wmonomorphism-restriction -Wmissing-home-modules       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-matches       #-}
{-# OPTIONS_GHC -fno-warn-unused-top-binds -fno-warn-unused-local-binds #-}
-- {-# OPTIONS_GHC -ddump-minimal-imports                               #-}

{-# LANGUAGE LambdaCase, ScopedTypeVariables #-}

import Test.QuickCheck

data Void

--          |Void| == 0
--            |()| == 1
--          |Bool| == 2
--       |Maybe a| == 1 + |a|
-- |Either a Bool| == |a| + 2 [nb. this is why Either is a Sum type]
--     |(a, Bool)| == |a| * b [nb. tuple is a Product type]
--        |a -> b| == |b|^|a|
--  |Bool -> Bool| == 2^2 == 4, i.e. id, not, const True, const False
--
-- Ex 1.2-i
-- |Either Bool (Bool, Maybe Bool) -> Bool| == 2 ^ (2 + (2 * (1 + 2)))
--                                          == 256
--
-- Ex. 1.4-i
-- Prove       (a^b)^c == a^(b*c)
--       c -> (b -> a) == (b, c) -> a
--
-- provide a function of type (b -> c -> a) -> (b, c) -> a [uncurry]
--                        and ((b, c) -> a) -> b -> c -> a [curry]
-- 
-- (|a|^|b|)^|c| == |a|^(|b|*|c|)
--
-- (b -> c -> a) -> (b, c) -> a
-- a^(b*c)^(a^(c^b))
--
-- ((b, c) -> a) -> b -> c -> a
-- a^(c^(b^(a^(b*c))))

curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f = \x y -> f (x, y)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f = \(x, y) -> f x y

-- Ex 1.4-ii
-- Prove      a^b * a^c == a^(b+c)
--     (b -> a, c -> a) == (Either b c) -> a


uncurriedEither :: (b -> a, c -> a) -> (Either b c -> a)
uncurriedEither (fba, fca) = \case
  Left x  -> fba x
  Right x -> fca x
  
unEither :: (Either b c -> a) -> (b -> a, c -> a)
unEither f = (f . Left, f . Right)

-- Ex 1.4-iii
-- Prove (a*b)^c == a^c * b^c
--   c -> (a, b) == (c -> a, c -> b)

-- TODO: name?
f1 :: (c -> (a, b)) -> (c -> a, c -> b)
f1 f = (fst . f, snd . f)

f2 :: (c -> a, c -> b) -> (c -> (a, b))
f2 (ca, cb) = \c -> (ca c, cb c)

-- (a, Either b c)
-- a*b a*c
-- Either (a, b)

prop_curryIso :: Int -> Int -> Bool
prop_curryIso x y = iso x y == orig x y
  where
    iso = curry' . uncurry' $ orig
    orig :: Int -> Int -> Int = (-)

main :: IO ()
main = do
  quickCheck prop_curryIso
