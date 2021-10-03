module Test.MySolutions where

import Prelude

import Control.Alt (alt, (<|>))
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), decodeJson, encodeJson, fromString, jsonParser, printJsonDecodeError)
import Data.Argonaut.Core as A
import Data.Argonaut.Decode.Decoders (decodeArray, decodeInt, decodeString, decodeTuple)
import Data.Argonaut.Decode.Generic (genericDecodeJson)
import Data.Argonaut.Encode.Generic (genericEncodeJson)
import Data.Bifunctor (lmap, bimap)
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Generic.Rep (class Generic)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Pair (Pair(..))
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Data.String (trim)
import Data.Tuple (Tuple(..))
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

valuesOfMap :: Map.Map String Int -> Either JsonDecodeError (Set Int)
valuesOfMap = encodeJson >>> valuesOfMapImpl >>> decodeJson

valuesOfMapGeneric ::
  ∀ k v.
  EncodeJson k =>
  EncodeJson v =>
  DecodeJson v =>
  Ord k =>
  Ord v =>
  Map.Map k v -> Either JsonDecodeError (Set v)
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

decodePairFn ::
  forall a.
  (Json -> Either JsonDecodeError a) ->
  Json ->
  Either JsonDecodeError (PairWrap a)
decodePairFn decoder json = decodeArray Right json >>= f
  where
  f :: Array Json -> Either JsonDecodeError (PairWrap a)
  f = case _ of
    [ a, b ] -> PairWrap <$> (Pair <$> decoder a <*> decoder b)
    _ -> Left $ TypeMismatch "Pair"

instance decodePair :: DecodeJson a => DecodeJson (PairWrap a) where
  --   decodeJson json = decodePair decodeJson json
  decodeJson json = decodePairFn decodeJson json

pairWrapToPair :: ∀ a. PairWrap a -> Pair a
pairWrapToPair (PairWrap p) = p

quadraticRootsSafe :: Quadratic -> Either JsonDecodeError (Pair Complex)
quadraticRootsSafe = encodeJson >>> quadraticRootsSafeImpl >>> decodeJson >>> map pairWrapToPair

-- parseAndDecodeArray2D string -> Either String (Array (Array Int))
foreign import _myJsonParse :: ∀ a. Fn3 (Json -> a) (String -> a) String a

myJsonParse :: String -> Either String Json
myJsonParse = runFn3 _myJsonParse Right Left

parseAndDecodeArray2D :: String -> Either String (Array (Array Int))
parseAndDecodeArray2D str = do
  json <- myJsonParse str
  lmap printJsonDecodeError $ decodeJson $ json

-- 6 binary tree
data Tree a
  = Leaf a
  | Branch (Tree a) (Tree a)

derive instance genericTree :: Generic (Tree a) _

-- decode, encode Tree
instance encodeJsonTree :: EncodeJson a => EncodeJson (Tree a) where
  encodeJson :: Tree a -> Json
  encodeJson t = genericEncodeJson t

instance decodeJsonTree :: DecodeJson a => DecodeJson (Tree a) where
  decodeJson :: Json -> Either JsonDecodeError (Tree a)
  decodeJson t = genericDecodeJson t

instance showJsonTree :: Show a => Show (Tree a) where
  show t = genericShow t

derive instance eqJsonTree :: Eq a => Eq (Tree a)

-- int or string
data IntOrString
  = IntOrString_Int Int
  | IntOrString_String String

derive instance genericIntOrString :: Generic (IntOrString) _

derive instance eqIntOrString :: Eq IntOrString

instance showIntOrString :: Show IntOrString where
  show ios = genericShow ios

instance encodeIntOrString :: EncodeJson IntOrString where
  encodeJson (IntOrString_Int i) = encodeJson i
  encodeJson (IntOrString_String s) = encodeJson s


instance decodeIntOrString :: DecodeJson IntOrString where
    decodeJson ios =
        foldr alt (Left $ TypeMismatch "not int or string")
            [ map IntOrString_Int $ decodeJson ios
            , map IntOrString_String $ decodeJson ios
            ]

--   decodeJson ios =
--     genericDecodeJson ios 
--       <|> (do
--         i <- decodeInt ios
--         pure $ IntOrString_Int i
--       )
--       <|> ( do
--             s <- decodeString ios
--             pure $ IntOrString_String s
--         )
