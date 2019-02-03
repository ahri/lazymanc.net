#!/usr/bin/env stack
{- stack --resolver lts-13.4 script
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

{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, QuasiQuotes, LambdaCase #-}
{-# LANGUAGE DataKinds, TypeOperators, KindSignatures, TypeFamilies #-}

import Test.QuickCheck
import GHC.TypeLits

{-
 - 3-i.
 -}

newtype T1 a = T1 (Int -> a)
newtype T2 a = T2 (a -> Int)
newtype T3 a = T3 (a -> a)
newtype T4 a = T4 ((Int -> a) -> Int)
newtype T5 a = T5 ((a -> Int) -> Int)

-- Y
instance Functor T1 where
    fmap (g :: a -> b) (T1 (f :: Int -> a)) = T1 h
      where (h :: Int -> b) = g . f

-- N
instance Functor T2 where
    fmap (g :: a -> b) (T2 (f :: a -> Int)) = T2 h
      where (h :: b -> Int) = \b -> error "can't turn a into b"

-- N
instance Functor T3 where
    fmap (g :: a -> b) (T3 (f :: a -> a)) = T3 h 
      where (h :: b -> b) = id -- TODO: ummm this compiles but doesn't use `f' or `g'

-- N
instance Functor T4 where
    fmap (g :: a -> b) (T4 (f :: (Int -> a) -> Int)) = T4 h
      where (h :: (Int -> b) -> Int) = \(h' :: Int -> b) -> error "can't turn a into b"

-- Y
instance Functor T5 where
    fmap (g :: a -> b) (T5 (f :: (a -> Int) -> Int)) = T5 h
      where (h :: (b -> Int) -> Int) = \(h' :: b -> Int) -> f $ h' . g
