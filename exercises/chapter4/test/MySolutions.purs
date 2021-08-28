module Test.MySolutions where

import Prelude

import Control.Alternative (guard)
import Data.Array (filter, head, length, null, tail, (..))
import Data.Maybe (fromMaybe)
import Data.Ord (greaterThanOrEq, lessThan, lessThanOrEq)

-- Note to reader: Add your solutions to this file

isEven :: Int -> Boolean
-- isEven n = n / 2 * 2 == n
isEven n = 
    if n < 0 then
        isEven (-n)
    else if n == 0 then
        true
    else if n == 1 then
        false
    else
        isEven $ n - 2


-- countEven
countEven :: Array Int -> Int
-- countEven = filter isEven >>> length
countEven ary =
    if null ary then
        0
    else if isEven $ fromMaybe 1 $ head ary then
        1 + (countEven $ fromMaybe [] $ tail ary)
    else
        countEven $ fromMaybe [] $ tail ary

-- squared :: ∀ a. Ring a => Eq a => Array a -> Array a
-- squared = map (\n -> n * n)
squared :: Array Number -> Array Number
squared = map (\n -> n * n)

keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter $ lessThanOrEq 0.0

infix 4 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite ary = (lessThanOrEq 0.0) <$?> ary

myFactors :: Int -> Array (Array Int)
myFactors n = do
    i <- 1 .. n
    j <- i .. n
    guard $ i * j == n
    pure [i, j]

isPrime :: Int -> Boolean
isPrime n = n > 1 && length (myFactors n) == 1