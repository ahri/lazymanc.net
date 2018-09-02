{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}

module C16 where

import           Data.Functor ((<$>))

c16 :: IO ()
c16 = do
    e' <- e
    print e'

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = read . ("123"++) . show <$> ioi
    in (*3) <$> changed

newtype Identity a = Identity a
instance Functor Identity where
    fmap f (Identity x) = Identity $ f x

data Pair a = Pair a a
instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

data Two a b = Two a b
instance Functor (Two a) where
    fmap f (Two x y) = Two x $ f y

data Three a b c = Three a b c
instance Functor (Three a b) where
    fmap f (Three x y z) = Three x y (f z)

data Three' a b = Three' a b b
instance Functor (Three' a) where
    fmap f (Three' x y z) = Three' x (f y) (f z)

data Four a b c d = Four a b c d
instance Functor (Four a b c) where
    fmap f (Four x y z w) = Four x y z $ f w

data Four' a b = Four' a a a b
instance Functor (Four' a) where
    fmap f (Four' x y z w) = Four' x y z $ f w

-- TODO: quickcheck above

data Sum b a
    = First a
    | Second b

instance Functor (Sum b) where
    fmap f (First a)  = First $ f a
    fmap _ (Second b) = Second b

data Company a c b
    = DeepBlue a c
    | Something b

instance Functor (Company a c) where
    fmap f (Something b)  = Something $ f b
    fmap _ (DeepBlue a c) = DeepBlue a c

data More b a
    = L a b a
    | R b a b

instance Functor (More b) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'

data Quant a b
    = Finance
    | Desk a
    | Bloor b

instance Functor (Quant a) where
    fmap _ Finance   = Finance
    fmap _ (Desk x)  = Desk x
    fmap f (Bloor b) = Bloor $ f b

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)
newtype Flipp f a b c = Flipp (f c b a) deriving (Eq, Show)
newtype NoFlip f a b = NoFlip (f a b) deriving (Eq, Show)

newtype K a b = K a deriving (Show)
instance Functor (K a) where
    fmap _ (K x) = K x

instance Functor (Flip K a) where
    fmap f (Flip (K x)) = Flip . K $ f x

-- instance Functor (Flip (,) a) where
--     fmap f (Flip (x, y)) = Flip (f x, y)

instance Functor ((,,) a b) where
    fmap f (x, y, z) = (x, y, f z)

instance Functor (Flipp (,,) a b) where
    fmap f (Flipp (x, y, z)) = Flipp (f x, y, z)

newtype EvilGoateeConst a b = GoatyConst b
instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst $ f b

newtype LiftItOut f a = LiftItOut (f a)
instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut x) = LiftItOut $ f <$> x

newtype Mu f = InF { outF :: f (Mu f) }
x = InF Nothing
y = InF (K 1)
