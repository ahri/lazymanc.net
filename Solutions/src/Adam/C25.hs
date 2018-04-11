{-# LANGUAGE InstanceSigs #-}

module C25 where

newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose x) = Compose $ (fmap . fmap) f x

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure = Compose . pure . pure

    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    (Compose f) <*> (Compose a) = Compose $ fmap (<*>) f <*> a
    -- NB. Compose $ f <*> a gave me:
    --  Expected type: f (g a -> g b)
    --  Actual type: f (g (a -> b))
    --
    --  so by looking at the general form of <*>:
    --    (<*>) :: Applicative f => f (a -> b) -> f a -> f b
    --  we can see that by partially applying, the `f` gets distributed over 
    --  the domain and codomain of the contained function, giving us just
    --  f a -> f b, which is what we need to get the expected type above
