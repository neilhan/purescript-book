module Test.MySolutions where

import Prelude

import Data.Argonaut (Json, JsonDecodeError, encodeJson, decodeJson)
import Data.Either (Either)
import Data.Function.Uncurried (Fn3)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Pair (Pair(..))
import Data.Set (Set)
import Test.Examples (Complex, Quadratic, Undefined)


-- Note to reader: Add your solutions to this file
foreign import volumeFn :: Fn3 Number Number Number Number

foreign import volumeArrow :: Number -> Number -> Number -> Number

foreign import cumulativeSumsComplex :: Array Complex -> Array Complex

foreign import quadraticRootsPair :: (∀ a. a -> a -> Pair a) -> Quadratic -> Pair Complex

quadraticRoots :: Quadratic -> Pair Complex
quadraticRoots q = quadraticRootsPair Pair q

foreign import toMaybeImpl :: ∀ a. (∀ x. x -> Maybe x) -> (∀ x. Maybe x) -> Undefined a -> Maybe a

toMaybe :: ∀ a. Undefined a -> Maybe a
toMaybe ua = toMaybeImpl Just Nothing ua

foreign import valuesOfMapImpl :: Json -> Json

valuesOfMap :: Map String Int -> Either JsonDecodeError (Set Int)
valuesOfMap = encodeJson >>> valuesOfMapImpl >>> decodeJson