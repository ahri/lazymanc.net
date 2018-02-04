module ChrisMonoidLaws where

import Test.QuickCheck
import Data.Monoid
import qualified Data.Semigroup as S
import ChrisC15

chrisMonoidLawsTest :: IO ()
chrisMonoidLawsTest = do
    -- use verboseCheck to see what is tested
    quickCheck (prop_monoidAssociativity :: String -> String -> String -> Bool)
    quickCheck (prop_monoidAssociativity' mappend :: String -> String -> String -> Bool)
    quickCheck (prop_monoidIdentity :: String -> Bool)
    quickCheck (prop_monoidIdentity' :: String -> Bool)
    -- quickCheck (prop_communicativity :: String -> String -> Bool)
    quickCheck (prop_semigroupAssociativity :: String -> String -> String -> Bool)
    quickCheck (prop_semigroupAssociativity :: Trivial -> Trivial -> Trivial -> Bool)

-- For something to be a monoid, it must pass these checks

prop_monoidAssociativity :: (Eq m, Monoid m) => m -> m -> m -> Bool
prop_monoidAssociativity a b c = a <> (b <> c) == (a <> b) <> c

prop_monoidAssociativity' :: (Eq m, Monoid m) => (m -> m -> m) -> m -> m -> m -> Bool
prop_monoidAssociativity' f a b c = a `f` (b `f` c) == (a `f` b) `f` c

prop_monoidIdentity :: (Eq m, Monoid m) => m -> Bool
prop_monoidIdentity a = (a <> mempty) == a

prop_monoidIdentity' :: (Eq m, Monoid m) => m -> Bool
prop_monoidIdentity' a = (mempty <> a) == a

-- Not needed for a monoid: communicativity, but let's test it anyway
-- Communicativity for lists/strings does not hold
--  i.e. "ab" /= "ba"
--prop_communicativity :: (Eq m, Monoid m) => m -> m -> Bool
--prop_communicativity a b = a <> b == b <> a

-- Semigroup testing
prop_semigroupAssociativity :: (Eq m, S.Semigroup m) => m -> m -> m -> Bool
prop_semigroupAssociativity a b c = (a S.<> b) S.<> c == a S.<> (b S.<> c)
