module Test.MySolutions where

import Prelude

import Control.Apply (lift2)
import Data.AddressBook (Address, address)
import Data.AddressBook.Validation (Errors, matches)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Traversable (class Foldable, class Traversable, foldMap, foldl, foldr, sequence, traverse)
import Data.Validation.Semigroup (V)

-- Note to reader: Add your solutions to this file

addMaybe :: ∀ a f. Apply f => Semiring a => f a -> f a -> f a
addMaybe = lift2 (+)

subMaybe :: ∀ a f. Apply f => Ring a => f a -> f a -> f a
subMaybe= lift2 (-)

mulMaybe :: ∀ a f. Apply f => Semiring a => f a -> f a -> f a
mulMaybe= lift2 (*)

divMaybe :: ∀ a f. Apply f => EuclideanRing a => f a -> f a -> f a 
divMaybe= lift2 (/)

-- all apply type
combineMaybe :: ∀ a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe (Just fa) = map pure fa
combineMaybe Nothing = pure Nothing

-- Validation
stateRegex :: Regex
stateRegex = unsafeRegex "^[A-Za-z]{2}$" noFlags

nonEmptyRegex  :: Regex
nonEmptyRegex = unsafeRegex "[^ \\t\\n\\r]+" noFlags

validateAddressImproved :: Address -> V Errors Address
validateAddressImproved a = ado
    street <- matches "Street" nonEmptyRegex a.street
    city <- matches "City" nonEmptyRegex a.city
    state <- matches "State" stateRegex a.state
    in address street city state

-- Eq Show for a Tree
data Tree a = Leaf | Branch (Tree a) a (Tree a)

derive instance eqTree :: Eq a => Eq (Tree a)

derive instance genericTree :: Generic (Tree a) _

instance showTree :: Show a => Show (Tree a) where
    show t = genericShow t -- needs t, will cause stack issue otherwise

-- traverse Tree
-- need functor, foldable instances
instance functorTree :: Functor Tree  where
  map :: ∀ a b. (a -> b) -> Tree a -> Tree b
  map _ Leaf = Leaf
  map f (Branch t1 a t2) = Branch (map f t1) (f a) (map f t2)

instance foldableTree :: Foldable Tree where
  foldr :: ∀ a b. (a -> b -> b) -> b -> Tree a -> b
  foldr _ acc Leaf = acc
  foldr f acc (Branch t1 v t2) = foldr f (f v (foldr f acc t2)) t1

  foldl :: ∀ a b. (b -> a -> b) -> b -> Tree a -> b
  foldl _ acc Leaf = acc
  foldl f acc (Branch t1 a t2) = foldl f (f (foldl f acc t1) a) t2

  foldMap :: ∀ a m. Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Leaf = mempty
  foldMap f (Branch t1 v t2) = (foldMap f t1) <> f v <> (foldMap f t2)

-- class (Functor t, Foldable t) <= Traversable t where
--   traverse :: ∀ a b m. Applicative m => (a -> m b) -> t a -> m (t b)
--   sequence :: ∀ a m. Applicative m => t (m a) -> m (t a)
instance traversableTree :: Traversable Tree where
  traverse :: ∀ a b m. Applicative m => (a -> m b) -> Tree a -> m (Tree b)
  traverse _ Leaf = pure Leaf
  traverse f (Branch t1 v t2) = Branch <$> (traverse f t1) <*> (f v) <*> (traverse f t2)

  sequence :: ∀ a m. Applicative m => Tree (m a) -> m (Tree a)
  sequence Leaf = pure Leaf
  sequence (Branch t1 v t2) = Branch <$> (sequence t1) <*> v <*> (sequence t2)
