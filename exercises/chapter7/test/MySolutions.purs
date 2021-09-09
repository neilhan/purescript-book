module Test.MySolutions where

import Prelude

import Control.Apply (lift2)
import Data.AddressBook (Address, address)
import Data.AddressBook.Validation (Errors, matches)
import Data.Maybe (Maybe(..))
import Data.String.Regex (Regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Validation.Semigroup (V(..))

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
