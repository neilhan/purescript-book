module Test.MySolutions where

import Prelude

import Data.Function.Uncurried (Fn3)
import Data.Pair (Pair(..))
import Test.Examples (Complex, Quadratic)


-- Note to reader: Add your solutions to this file
foreign import volumeFn :: Fn3 Number Number Number Number

foreign import volumeArrow :: Number -> Number -> Number -> Number

foreign import cumulativeSumsComplex :: Array Complex -> Array Complex

foreign import quadraticRootsPair :: (∀ a. a -> a -> Pair a) -> Quadratic -> Pair Complex

quadraticRoots :: Quadratic -> Pair Complex
quadraticRoots q = quadraticRootsPair Pair q