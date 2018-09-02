module ChrisC20 where

import Data.Foldable
import Data.Monoid

-- class Foldable t where
-- {-# MINIMAL foldMap | foldr ~-}

nums1 = map Sum [1, 2, 3, 4, 5]

nums2 = map Product [1, 2, 3, 4, 5]

main' :: IO ()
main' = do
    let lst1 = ["fold ", "takes a monoid ", "which needs foldr defined for it"]
    print $ foldr (++) "" lst1 == fold lst1
    print $ fold nums1 == foldr (+) 0 nums1
    print $ fold nums2 == foldr (*) 1 nums2
    print $ "The first argument of foldMap must map each element of the structure to a Monoid"
    print $ foldMap Product [1..5]
    print $ foldMap All [(3 == 3), (9 > 5)]
    print $ foldMap (*5) $ map Product [1..5]
    print $ foldr (*) 1 [1..5]
    print $ "foldMap = foldr (mappend . f) mempty"
    let sum2 :: Sum Int
        sum2 = 2
    print $ foldMap (+2) (Constant sum2)
    print $ myFoldMap (+2) $ map Sum [1..5]

data Identity a = Identity a

instance Foldable Identity where
    foldr f z (Identity a) = f a z
    foldl f z (Identity a) = f z a
    foldMap f (Identity a) = f a

data Constant a b = Constant b

instance Foldable (Constant a) where
    foldMap f (Constant b) = f b

data Two a b = Two a b

--instance Foldable (Two a) where
    --    foldMap f (Two a b) = f a b

myFoldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
myFoldMap f = foldr (mappend . f) mempty

