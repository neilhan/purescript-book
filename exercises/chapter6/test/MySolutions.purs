module Test.MySolutions where

import Data.Generic.Rep
import Data.Show.Generic
import Prelude

import Data.Array (length, nub, nubByEq, nubEq)
import Data.Foldable (class Foldable, foldl, foldr, foldMap)
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
