module Test.MySolutions where

import Prelude

import Control.Alternative (guard)
import Data.Array (cons, filter, head, length, null, tail, (..))
import Data.Int (quot)
import Data.Maybe (fromMaybe)
import Data.Ord (greaterThanOrEq, lessThan, lessThanOrEq)
import Data.Tuple (Tuple)

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

cartesianProduct :: ∀ a. Array a -> Array a -> Array (Array a)
cartesianProduct arya aryb = do
    i <- arya
    j <- aryb
    [[i, j]]


triples :: Int -> Array (Array Int)
triples n = do
    i <- 1 .. n
    j <- i .. n
    k <- j .. n
    guard $ i * i + j*j == k * k
    pure [i, j, k]

factorize :: Int -> Array Int
factorize n = f' 2 n []
    where
        f' :: Int -> Int -> Array Int -> Array Int
        f' _ 1 result = result
        f' divisor devidend result = 
            let isFactor i nn = (nn/i) * i == nn
            in
                if isFactor divisor devidend then
                    f' divisor (quot devidend divisor) (cons divisor result)
                else
                    f' (divisor + 1) devidend result