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

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap f (Compose x) = (foldMap . foldMap) f x

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse f (Compose x) = Compose <$> (traverse . traverse) f x

class Bifunctor p where
    {-# MINIMAL bimap | first, second #-}

    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
    bimap f g = first f . second g

    first :: (a -> b) -> p a c -> p b c
    first f = bimap f id

    second :: (c -> d) -> p a c -> p a d
    second f = bimap id f

data Deux a b = Deux a b
instance Bifunctor Deux where
    bimap f g (Deux x y) = Deux (f x) (g y)

data Const a b = Const a
instance Bifunctor Const where
    bimap f _ (Const x) = Const $ f x

data Drei a b c = Drei a b c
instance Bifunctor (Drei a) where
    bimap f g (Drei x y z) = Drei x (f y) (g z)

data SuperDrei a b c = SuperDrei a b
instance Bifunctor (SuperDrei a) where
    bimap f _ (SuperDrei x y) = SuperDrei x (f y)

data SemiDrei a b c = SemiDrei a
instance Bifunctor (SemiDrei a) where
    bimap _ _ (SemiDrei x) = SemiDrei x

data Quadriceps a b c d = Quadzzzz a b c d
instance Bifunctor (Quadriceps a b) where
    bimap f g (Quadzzzz a b c d) = Quadzzzz a b (f c) (g d)

instance Bifunctor Either where
    bimap f g x = case x of
        Left a  -> Left $ f a
        Right b -> Right $ g b
