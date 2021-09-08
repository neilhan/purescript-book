module Test.MySolutions where

import Prelude

import Control.Apply (lift2)
import Data.Maybe (Maybe(..))

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