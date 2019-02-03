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
 - 2.1.3
 - i.   Show       :: Type -> Constraint
 - ii.  Functor    :: (Type -> Type) -> Constraint
 - iii. Monad      :: (Type -> Type) -> Constraint
 - iv.  MonadTrans :: (Type -> Type) -> (Type -> Type) -> Constraint
 -}

 {-
  - 2.4-i
  -}

type family Not (x :: Bool) :: Bool where
    Not 'True = 'False
    Not 'False = 'True

foo :: 'True -> Not 'False
foo = undefined

tests :: IO ()
tests = quickCheck $ ((==)::Int -> Int -> Bool)

main :: IO ()
main = tests
