{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}

module C17 where

import           Data.Monoid ((<>))

data List a
    = Nil
    | Cons a (List a)
    deriving (Eq, Show, Functor, Foldable)

instance Monoid (List a) where
    mempty = Nil
    Nil `mappend` x = x
    x `mappend` Nil = x
    Cons x xs `mappend` y = Cons x $ xs `mappend`  y

instance Applicative List where
    pure x = Cons x Nil
    Nil <*> _          = Nil
    _ <*> Nil          = Nil
    -- Cons f fs <*> xs = (f <$> xs) <> (fs <*> xs)
    fs <*> xs = foldMap (<$> xs) fs
