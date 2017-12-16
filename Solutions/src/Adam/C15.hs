module C15 where

import           Data.Monoid

c15 :: IO ()
c15 = do
    print $ Only (Sum 1) `mappend` Only (Sum 1)
    print $ Only (Product 4) `mappend` Only (Product 2)
    print $ Only (Sum 1) `mappend` Nada
    print $ Only [1] `mappend` Nada
    print $ Nada `mappend` Only (Sum 1)

data Optional a
    = Nada
    | Only a
    deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    mappend (Only x) (Only y) = Only $ x <> y
    mappend (Only x) _        = Only x
    mappend _ (Only x)        = Only x
    mappend _ _               = Nada

