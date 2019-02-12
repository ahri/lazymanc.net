#!/usr/bin/env stack
{- stack --resolver lts-13.4 script
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

{-# LANGUAGE OverloadedStrings, QuasiQuotes, LambdaCase #-}

{-# LANGUAGE ExistentialQuantification, FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds, DataKinds, GADTs, ScopedTypeVariables, TypeApplications, TypeFamilies, TypeOperators, UndecidableInstances #-}
import Data.Kind (Constraint, Type)

{-
 - 5.
 -}

data Expr a where
    LitInt  :: Int -> Expr Int
    LitBool :: Bool -> Expr Bool
    Add     :: Expr Int -> Expr Int -> Expr Int
    Not     :: Expr Bool -> Expr Bool
    If      :: Expr Bool -> Expr a -> Expr a -> Expr a

data Expr_ a
    = (a ~ Int)  => LitInt_ Int
    | (a ~ Bool) => LitBool_ Bool
    | (a ~ Int)  => Add_ (Expr_ Int) (Expr_ Int)
    | (a ~ Bool) => Not_ (Expr_ Bool)
    | If_ (Expr_ Bool) (Expr_ a) (Expr_ a)

-- data LitInt' = LitInt' Int
-- data LitBool' = LitBool' Bool

-- data Expr''
--     = LitInt'' LitInt'
--     | LitBool'' LitBool'
--     | Add'' LitInt' LitInt'
--     deriving (Show)

-- evalExpr'' :: Expr'' -> ???

evalExpr :: Expr a -> a
evalExpr (LitInt i)  = i
evalExpr (LitBool b) = b
evalExpr (Add x y)   = evalExpr x + evalExpr y
evalExpr (Not x)     = not $ evalExpr x
evalExpr (If b x y)  = if evalExpr b
    then evalExpr x
    else evalExpr y

evalExpr_ :: Expr_ a -> a
evalExpr_ (LitInt_ i)  = i
evalExpr_ (LitBool_ b) = b
evalExpr_ (Add_ x y)   = evalExpr_ x + evalExpr_ y
evalExpr_ (Not_ x)     = not $ evalExpr_ x
evalExpr_ (If_ b x y)  = if evalExpr_ b
    then evalExpr_ x
    else evalExpr_ y

tests :: IO ()
tests = do
    print . evalExpr $ If (Not $ LitBool True)
        (LitInt 1)
        (Add (LitInt 2) (LitInt 3))

    print . evalExpr_ $ If_ (Not_ $ LitBool_ True)
        (LitInt_ 1)
        (Add_ (LitInt_ 2) (LitInt_ 3))

    -- print $ LitInt' 8
