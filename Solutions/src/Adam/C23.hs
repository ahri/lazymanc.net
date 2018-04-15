{-# LANGUAGE InstanceSigs #-}

module C23 where

import           Data.Semigroup

newtype Moi s a =
    Moi { runMoi :: s -> (a, s) }

newtype Moi2 s a = Moi2 (s -> (a, s))
runMoi2 :: Moi2 s a -> s -> (a, s)
runMoi2 (Moi2 f) = f

instance Functor (Moi s) where
    fmap :: (a -> b) -> Moi s a -> Moi s b
    -- fmap f (Moi g) = Moi $ (\(a, s) -> (f a, s)) . g
    fmap f (Moi g) = Moi foo
        where
            foo s = (f a, s')
                where
                (a, s') = g s

instance Applicative (Moi s) where
    pure :: a -> Moi s a
    pure a = Moi $ \s -> (a, s)
    (<*>) :: Moi s (a -> b)
          -> Moi s a
          -> Moi s b
    (Moi f) <*> (Moi g) = Moi foo
        where
            foo s0 = (f' a, s2)
                where
                    (f', s1) = f s0
                    (a,  s2) = g s1

instance Monad (Moi s) where
    return = pure
    (>>=) :: Moi s a
          -> (a -> Moi s b)
          -> Moi s b
    (Moi f) >>= g = Moi foo
        where
            foo s0 = (b, s2)
                where
                    (a, s1) = f s0
                    (b, s2) = runMoi (g a) s1


-- newtype Foo a b = Foo { makeTuple :: a -> b -> (b, a) }

-- instance Functor (Foo a)
