-- | This module defines a datatype `Pair` together with a few useful instances
-- | and helper functions. Note that this is not just `Tuple a a` but rather a
-- | list with exactly two elements. Specifically, the `Functor` instance maps
-- | over both values (in contrast to the `Functor` instance for `Tuple a`).
module Data.Pair
  ( Pair(..)
  , (~)
  , fst
  , snd
  , curry
  , uncurry
  , swap
  ) where

import Prelude

import Data.Foldable (class Foldable)
import Data.Traversable (class Traversable)
import Data.Distributive (class Distributive)

import Test.QuickCheck.Arbitrary (class Arbitrary, arbitrary)

-- | A pair simply consists of two values of the same type.
data Pair a = Pair a a

infixl 6 Pair as ~

-- | Returns the first component of a pair.
fst ∷ ∀ a. Pair a → a
fst (x ~ _) = x

-- | Returns the second component of a pair.
snd ∷ ∀ a. Pair a → a
snd (_ ~ y) = y

-- | Turn a function that expects a pair into a function of two arguments.
curry ∷ ∀ a b. (Pair a → b) → a → a → b
curry f x y = f (x ~ y)

-- | Turn a function of two arguments into a function that expects a pair.
uncurry ∷ ∀ a b. (a → a → b) → Pair a → b
uncurry f (x ~ y) = f x y

-- | Exchange the two components of the pair
swap ∷ ∀ a. Pair a → Pair a
swap (x ~ y) = y ~ x

derive instance eqPair ∷ Eq a ⇒ Eq (Pair a)

derive instance ordPair ∷ Ord a ⇒ Ord (Pair a)

instance showPair ∷ Show a ⇒ Show (Pair a) where
  show (x ~ y) = "(" <> show x <> " ~ " <> show y <> ")"

instance functorPair ∷ Functor Pair where
  map f (x ~ y) = f x ~ f y

instance applyPair ∷ Apply Pair where
  apply (f ~ g) (x ~ y) = f x ~ g y

instance applicativePair ∷ Applicative Pair where
  pure x = x ~ x

instance bindPair ∷ Bind Pair where
  bind (x ~ y) f = fst (f x) ~ snd (f y)

instance monadPair ∷ Monad Pair

instance semigroupPair ∷ Semigroup a ⇒ Semigroup (Pair a) where
  append (x1 ~ y1) (x2 ~ y2) = (x1 <> x2) ~ (y1 <> y2)

instance monoidPair ∷ Monoid a ⇒ Monoid (Pair a) where
  mempty = mempty ~ mempty

instance foldablePair ∷ Foldable Pair where
  foldr f z (Pair x y) = x `f` (y `f` z)
  foldl f z (Pair x y) = (z `f` x) `f` y
  foldMap f (Pair x y) = f x <> f y

instance traversablePair ∷ Traversable Pair where
  traverse f (Pair x y) = Pair <$> f x <*> f y
  sequence (Pair mx my) = Pair <$> mx <*> my

instance distributivePair ∷ Distributive Pair where
  distribute xs = map fst xs ~ map snd xs
  collect f xs = map (fst <<< f) xs ~ map (snd <<< f) xs

instance arbitraryPair ∷ Arbitrary a ⇒ Arbitrary (Pair a) where
  arbitrary = Pair <$> arbitrary <*> arbitrary
