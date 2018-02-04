module ChrisC18 where

import Prelude
import Control.Monad (join)

myBind :: Monad m => (a -> m b) -> m a -> m b
myBind f x = join $ f <$> x

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x <- xs
    if even x
       then [x*x, x*x]
       else []

data Sum a b 
    = First a
    | Second b
    deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First x) = First x
    fmap f (Second x) = Second $ f x

instance Applicative (Sum a) where
    pure = Second
    _ <*> (First a) = First a 
    (First a) <*> _ = First a
    (Second f) <*> (Second a) = Second (f a)

instance Monad (Sum a) where
    return = pure
    Second a >>= f = f a

