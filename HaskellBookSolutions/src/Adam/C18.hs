module C18 where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data Nope a = NopeDotJpg deriving (Show, Eq)

instance Functor Nope where
    fmap _ _ = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    _ <*> _ = NopeDotJpg

instance Monad Nope where
    (>>=) _ _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
    arbitrary = pure <$> arbitrary

instance Eq a => EqProp (Nope a) where
    (=-=) = eq


data ReverseEither b a = Left' a | Right' b deriving (Show, Eq)

instance Functor (ReverseEither b) where
    fmap f (Left' x)  = Left' $ f x
    fmap _ (Right' x) = Right' x

instance Applicative (ReverseEither b) where
    pure = Left'
    (Right' x) <*> _ = Right' x
    _ <*> (Right' x) = Right' x
    (Left' f) <*> (Left' x) = Left' $ f x

instance Monad (ReverseEither b) where
    x >>= f = join $ f <$> x
        where join (Left' (Left' x)) = Left' x

instance (Arbitrary b, Arbitrary a) => Arbitrary (ReverseEither b a) where
    arbitrary = pure <$> arbitrary

instance (Eq b, Eq a) => EqProp (ReverseEither b a) where
    (=-=) = eq
