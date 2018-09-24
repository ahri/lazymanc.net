{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}

module MiguelC21 where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Data.Monoid((<>))
    
newtype Identity a = Identity a
                     deriving (Eq, Ord, Show, Functor, Foldable)

instance Traversable Identity where
    traverse f (Identity a) = Identity <$> (f a)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary

instance Eq a => EqProp (Identity a) where
    (=-=) = eq

newtype Constant a b =
    Constant { getConstant :: a } 
    deriving (Eq, Ord, Show, Functor, Foldable)

instance Traversable (Constant x) where
    traverse _ (Constant a) = pure $ Constant a -- wat? ignore function?

instance Arbitrary a => Arbitrary (Constant a b) where
    arbitrary = Constant <$> arbitrary

instance Eq a => EqProp (Constant a b) where
    (=-=) = eq
    
data Optional a = Nada
                | Yep a 
                deriving (Eq, Ord, Show, Functor, Foldable)

instance Traversable Optional where
    traverse _ Nada    = pure Nada
    traverse f (Yep a) = Yep <$> f a

instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = oneof [ Yep <$> arbitrary , return Nada ]

instance Eq a => EqProp (Optional a) where
    (Yep a) =-= (Yep b) = a `eq` b
    Nada    =-= Nada    = property True
    _       =-= _       = property False

data List a = Nil
            | Cons a (List a)
            deriving (Eq, Ord, Show, Functor, Foldable)

instance Traversable List where
    traverse _ Nil         = pure Nil
    traverse f (Cons x xs) = Cons <$> f x <*> (traverse f xs)

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = frequency [ (9, Cons <$> arbitrary <*> arbitrary)
                          , (1, return Nil) ]

instance Eq a => EqProp (List a) where
    (Cons a as) =-= (Cons b bs) = (a `eq` b) .&&. (as =-= bs)
    Nil         =-= Nil         = property True
    _           =-= _           = property False

data Three a b c = Three a b c
                   deriving (Eq, Ord, Show, Functor, Foldable)

instance Traversable (Three a b) where
    traverse f (Three a b c) = Three <$> pure a <*> pure b <*> f c

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = Three <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where
    (Three a b c) =-= (Three a' b' c') = (a `eq` a) .&&. (b `eq` b') .&&. (c `eq` c')

data Big a b = Big a b b
             deriving (Eq, Ord, Show, Functor, Foldable)

instance Traversable (Big a) where
    traverse f (Big a b b') = Big <$> pure a <*> f b <*> f b'

instance (Arbitrary a, Arbitrary b) => Arbitrary (Big a b) where
    arbitrary = Big <$> arbitrary <*> arbitrary <*> arbitrary

instance (Eq a, Eq b) => EqProp (Big a b) where
    (Big a b c) =-= (Big a' b' c') = (a `eq` a) .&&. (b `eq` b') .&&. (c `eq` c')   

data Tree a =
    Empty
    | Leaf a
    | Node (Tree a) a (Tree a)
    deriving (Eq, Show)

instance Functor Tree where
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node left a right) = Node (fmap f left) (f a) (fmap f right)

instance Foldable Tree where
    foldMap f (Leaf a) = f a
    foldMap f (Node left a right) = (foldMap f left) <> f a <> (foldMap f right)

instance Traversable Tree where
    traverse f (Leaf a) = Leaf <$> f a
    traverse f (Node left a right) =
        Node <$> (traverse f left) <*> f a <*> (traverse f right)

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = frequency [ (4, Node <$> arbitrary <*> arbitrary <*> arbitrary)
                          , (1, Leaf <$> arbitrary ) ]
        
instance Eq a => EqProp (Tree a) where
    (Node left a right) =-= (Node left' a' right') = (left =-= left') .&&. (a `eq` a') .&&. (right =-= right')
    (Leaf a)            =-= (Leaf a')              = a `eq` a'
    _                   =-= _                      = property False

miguelsChapter21 :: IO ()    
miguelsChapter21 = do
    quickBatch (traversable (undefined :: Identity (String, Int, [Int])))
    quickBatch (traversable (undefined :: Constant Int (Int, Int, [Int])))
    quickBatch (traversable (undefined :: Optional (Int, Bool, [Int])))
    quickBatch (traversable (undefined :: List (Int, Int, String)))
    quickBatch (traversable (undefined :: Three Int Char (Int, Int, [Int])))
    quickBatch (traversable (undefined :: Big Int (Int, String, [Int])))
    -- quickBatch (traversable (undefined :: Tree (Int, Int, [Int]))) -- messed up somewhere, this loops forever
