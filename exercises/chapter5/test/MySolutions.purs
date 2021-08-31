module Test.MySolutions where

import Prelude

factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (n-1)

-- 1st, n-th power
-- 2nd, k-th position
binomial :: Int -> Int -> Int
binomial n k 
    | k > n = 0
    | n == 0 = 1
    | n == 1 && k == 0 = 0
    | n == 1 && k == 1 = 1
    | otherwise = factorial n / (factorial k) / (factorial (n-k))

-- 1st, n-th power
-- 2nd, k-th position
-- pascal computing binomial using Pascal's rule
pascal :: Int -> Int -> Int
pascal _ 0 = 1
pascal 0 _ = 0
pascal n k = pascal (n-1) (k-1) + pascal (n-1) k