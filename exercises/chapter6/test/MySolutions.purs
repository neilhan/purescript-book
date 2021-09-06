module Test.MySolutions where

import Data.Generic.Rep
import Data.Show.Generic
import Prelude

import Data.Array (length, nub, nubByEq, nubEq, reverse, sort)
import Data.Array.Partial (head)
import Data.Foldable (class Foldable, foldl, foldr, foldMap)
import Data.Hashable (class Hashable, hashEqual, hashCode)
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Newtype (class Newtype, over2, wrap)

newtype Point = Point {x :: Number, y :: Number}

-- Note to reader: Add your solutions to this file

-- instance showPoint :: Show Point where
--     show (Point p) = "{x:" <> (show p.x) <> "," <> "y:" <> (show p.y) <> "}"
-- --   show (Point p) =
-- --     "(" <> show p.x <> ", " <> show p.y <> ")"

instance showPoint :: Show Point where
    show (Point p) = "(" <> (show p.x) <> ", " <> (show p.y) <> ")"
    -- show {x, y} = "{x:" <> (show x) <> "," <> "y:" <> (show y) <> "}"


newtype Complex = Complex {real :: Number, imaginary :: Number}

instance showComplex :: Show Complex where
    show (Complex {real, imaginary}) 
        | (imaginary < 0.0) = (show real) <> (show imaginary) <> "i"
        | otherwise         = (show real) <> "+" <> (show imaginary) <> "i"

-- derive instance genericComplex :: Generic Complex _
-- instance showComplex :: Show Complex where
--     show = genericShow

-- Eq instance
derive instance eqComplex :: Eq Complex

-- Semiring instance for Complex
-- instance semiringComplex' :: Semiring Complex where
--   zero = Complex {real:0.0, imaginary:0.0}
--   one  = Complex {real:1.0, imaginary:0.0}
--   add (Complex {real:r1, imaginary:i1}) (Complex {real:r2, imaginary:i2}) = 
--     Complex {real: (r1+r2), imaginary: (i1+i2)}
--   mul (Complex {real:r1, imaginary:i1}) (Complex {real:r2, imaginary:i2}) =
--     Complex {real: (r1*r2-i1*i2), imaginary: (r2*i1+r1*i2)}

-- try using over and wrap
derive instance newtypeComplex :: Newtype Complex _

instance semiringComplex :: Semiring Complex where
  zero = wrap zero -- {real:0.0, imaginary:0.0}
  one = wrap {real:1.0, imaginary:0.0}
  add = over2 Complex add
  mul (Complex {real:r1, imaginary:i1}) (Complex {real:r2, imaginary:i2}) =
    Complex {real: (r1*r2-i1*i2), imaginary: (r2*i1+r1*i2)}

derive newtype instance ringComplet :: Ring Complex

-- 4. derive a ring
data Shape
  = Circle Point Number
  | Rectangle Point Number Number
  | Line Point Point
  | Text Point String

-- derive instance Shape :: Ring Shape _
derive instance genericShape :: Generic Shape _

instance showShape :: Show Shape where
    show = genericShow

-- eq for NonEmpty
data NonEmpty a = NonEmpty a (Array a)

derive instance eqNonEmpty :: Eq a => Eq (NonEmpty a)

derive instance genericNonEmpty :: Generic (NonEmpty a) _

instance showNonEmpty :: Show a => Show (NonEmpty a) where
    show = genericShow

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
    append (NonEmpty e1 a1) (NonEmpty e2 a2) = NonEmpty e1 (a1 <> [e2] <> a2)

derive instance functorNonEmpty :: Functor NonEmpty
-- instance functorNonEmpty ::Functor (NonEmpty a) where
--     map f (NonEmpty e a) = NonEmpty (f e) (map f a)

data Extended a = Infinite | Finite a

derive instance eqExtended :: Eq a => Eq (Extended a)

instance ordExtended :: (Eq a, Ord a) => Ord (Extended a) where
    compare Infinite Infinite = EQ
    compare Infinite (Finite _) = GT
    compare (Finite _) Infinite  = LT
    compare (Finite a) (Finite b) = compare a b

instance foldableNonEmpty :: Foldable NonEmpty where
    foldl :: ∀ a b. (b -> a -> b) -> b -> (NonEmpty a) -> b
    foldl fn st (NonEmpty e arr) = foldl fn st ([ e ] <> arr)
    foldr :: ∀ a b. (a -> b -> b) -> b -> (NonEmpty a)-> b
    foldr fn st (NonEmpty e arr) = foldr fn st ([ e ] <> arr) -- foldr fn (fn e b) a
    foldMap :: ∀ a m. Monoid m => (a -> m) -> (NonEmpty a) -> m
    foldMap fn (NonEmpty val arr) = foldMap fn ([ val ] <> arr)

-- f type->type?
data OneMore f a = OneMore a (f a)

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
    -- foldl :: ∀ a b. (b -> a -> b) -> b -> (OneMore f a) -> b
    foldl fn st (OneMore e arr) = foldl fn (fn st e) arr
    -- foldl func st (OneMore val more) = foldl func firstB more
    --     where
    --     firstB = (func st val)
    -- foldr :: ∀ a b. (a -> b -> b) -> b -> (OneMore f a)-> b
    foldr fn st (OneMore e arr) = fn e (foldr fn st arr)
    -- foldr func st (OneMore val more) = func val lastB
    --     where
    --     lastB = foldr func st more
    -- foldMap :: ∀ a m. Monoid m => (a -> m) -> (OneMore f a) -> m
    foldMap fn (OneMore e arr) = (fn e) <> foldMap fn arr
    -- foldMap func (OneMore val more) = (func val) <> (foldMap func more)

-- dedupShape
derive instance eqShape :: Eq Shape
derive newtype instance eqPoint :: Eq Point

dedupShapes :: Array Shape -> Array Shape
dedupShapes = nubEq

derive instance ordShape :: Ord Shape
derive newtype instance ordPoint :: Ord Point

dedupShapesFast :: Array Shape -> Array Shape
dedupShapesFast = nub

-- partial function, max
unsafeMaximum :: Partial => Array Int -> Int
unsafeMaximum a = head $ reverse $ sort a

-- action class multiply
class Monoid m <= Action m a where
    act :: m -> a -> a

newtype Multiply = Multiply Int

instance semigroupMultiply :: Semigroup Multiply where
    append (Multiply a) (Multiply b) = Multiply (a * b)

instance monoidMultiply :: Monoid Multiply where
    mempty = Multiply 1

instance actionMultiplyInt :: Action Multiply Int where
    act (Multiply m) b = m * b

-- action multiply string
instance actionMultiplyString :: Action Multiply String where
    act (Multiply m) str = power str m

-- action multiply on array
instance actionArray :: Action m a => Action m (Array a) where
    act m ary = map (act m) ary

-- newtype Self ?
newtype Self m = Self m

derive newtype instance eqSelf :: Eq m => Eq (Self m)
derive newtype instance showSelf :: Show m => Show (Self m)

derive newtype instance eqMultiply :: Eq Multiply
derive newtype instance showMultiply :: Show Multiply

instance actionSelf :: Monoid m => Action m (Self m) where
    act m (Self m2) = Self $ m <> m2

-- hashable exercises
arrayHasDuplicates :: ∀ a. Hashable a => Array a -> Boolean
arrayHasDuplicates ary = length ary /= length (nubByEq (\a b -> hashEqual a b && a == b) ary)

newtype Hour = Hour Int
instance eqHour :: Eq Hour where
    eq (Hour n1) (Hour n2) = mod n1 12 == mod n2 12

instance hashableHour :: Hashable Hour where
    hash (Hour n) = hashCode $ mod n 12