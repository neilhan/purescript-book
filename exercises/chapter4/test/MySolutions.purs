module Test.MySolutions where

import Prelude

import Data.Array (head, null, tail)
import Data.Maybe (fromMaybe)

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