module Test.MySolutions where

import Prelude

import Control.Monad.Trans.Class (lift)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, encodeJson)
import Data.Argonaut.Decode.Decoders (decodeArray, decodeTuple)
import Data.Either (Either(..))
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

-- using Json as input output --------------------
foreign import valuesOfMapImpl :: Json -> Json

valuesOfMap :: Map String Int -> Either JsonDecodeError (Set Int)
valuesOfMap = encodeJson >>> valuesOfMapImpl >>> decodeJson

valuesOfMapGeneric ::
  ∀ k v.
  EncodeJson k =>
  EncodeJson v =>
  DecodeJson v =>
  Ord k =>
  Ord v =>
  Map k v -> Either JsonDecodeError (Set v)
valuesOfMapGeneric = encodeJson >>> valuesOfMapImpl >>> decodeJson

-- quadraticRoots with Json
foreign import quadraticRootsSetImpl :: Json -> Json

-- take Quadratic json, return Set of Complex
quadraticRootsSet :: Quadratic -> Either JsonDecodeError (Set Complex)
quadraticRootsSet = encodeJson >>> quadraticRootsSetImpl >>> decodeJson

-- safe quadraticRoots, JSON in out
foreign import quadraticRootsSafeImpl :: Json -> Json

-- need to be able to decode Pair
newtype PairWrap a
  = PairWrap (Pair a)

decodePairFn :: forall a
  . (Json -> Either JsonDecodeError a)
  -> Json
  -> Either JsonDecodeError (PairWrap a)
decodePairFn decoder json = decodeArray Right json >>= f
  where
  f :: Array Json -> Either JsonDecodeError (PairWrap a)
  f = case _ of
    [a, b] -> PairWrap <$> (Pair <$> decoder a <*> decoder b)
    _ -> Left $ TypeMismatch "Pair"

instance decodePair :: DecodeJson a => DecodeJson (PairWrap a) where
--   decodeJson json = decodePair decodeJson json
  decodeJson json = decodePairFn decodeJson json

pairWrapToPair :: ∀ a. PairWrap a -> Pair a
pairWrapToPair (PairWrap p) = p

quadraticRootsSafe :: Quadratic -> Either JsonDecodeError (Pair Complex)
quadraticRootsSafe = encodeJson >>> quadraticRootsSafeImpl >>> decodeJson >>> map pairWrapToPair
