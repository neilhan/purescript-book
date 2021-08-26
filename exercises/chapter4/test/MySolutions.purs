module Test.MySolutions where

import Prelude

import Data.Array (filter, length)

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
countEven = (filter isEven) >>> length