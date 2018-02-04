module ChrisC15 where

import qualified Data.Monoid as M
import qualified Data.Semigroup as S
import Test.QuickCheck.Arbitrary

data Booly a = False' | True'
    deriving (Eq, Show)

-- Conjunction
instance Monoid (Booly a) where
    mappend False' _ = False'
    mappend _ False' = False'
    mappend True' True' = True'
-- False' <> False' -> False'
-- True' <> False' -> False'
-- False' <> True' -> False'
-- True' <> True' -> True'

data Optional a
    = Nada
    | Only a
    deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
    mempty = Nada
    mappend (Only a) (Only a') = Only $ mappend a a'
    mappend (Only a) Nada = Only a
    mappend Nada (Only a) = Only a
    mappend Nada Nada = Nada
-- mappend Nada (Only a) -> Only a -- identity 


-- Mad libbing

type Verb = String
type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbin' :: Exclamation
    -> Adverb
    -> Noun
    -> Adjective
    -> String
madlibbin' e adv noun adj =
    e
    M.<> "! he said " M.<>
    adv M.<> " as he jumped into his car " M.<>
    noun M.<> " and drove off with his " M.<>
    adj M.<> " wife."

madlibbinBetter' :: Exclamation
    -> Adverb
    -> Noun
    -> Adjective
    -> String
madlibbinBetter' e adv noun adj = mconcat $
    [e, "! he said ", adj, " as he jumped into his car ", noun, " and drove off with his ",
    adj, " wife."]

-- Exercises
data Trivial = Trivial deriving (Eq, Show)

instance S.Semigroup Trivial where
    Trivial <> Trivial = Trivial

instance Arbitrary Trivial where
    arbitrary = return Trivial