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

{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, QuasiQuotes, LambdaCase #-}
{-# LANGUAGE DataKinds, TypeOperators, KindSignatures, TypeFamilies #-}
{-# LANGUAGE TypeApplications, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

import Data.Typeable

{-
 - 4.3.
 -}

typeName :: forall a. Typeable a => String
typeName = show . typeRep $ Proxy @a

type family AlwaysUnit a where
    AlwaysUnit a = ()

foo :: AlwaysUnit a -> b
foo = undefined

bar :: b -> AlwaysUnit a -> b
bar = undefined

baz :: Show a => AlwaysUnit a -> String
baz = undefined

tests :: IO ()
tests = do
    print $ typeName @String
    print $ typeName @(AlwaysUnit Int -> Char)
    print $ typeName @(Char -> AlwaysUnit Int -> Char)
