module Test.MySolutions where

import Prelude

import Control.Alternative (guard)
import Data.Array (concat, concatMap, cons, filter, foldl, head, length, null, sortBy, tail, (..))
import Data.Int (quot)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Ord (lessThanOrEq)
import Data.Ordering (invert)
import Data.Path (Path(..), filename, isDirectory, ls)
import Data.String (Pattern, contains)
import Data.String.Utils (endsWith)
import Test.Examples (allFiles)

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

allTrue :: Array Boolean -> Boolean
allTrue = foldl (\coll a -> coll && a) true

fibTailRec :: Int -> Int
fibTailRec n = fib' 2 1 0 -- start from 2, need to do this. 
    where 
        fib' :: Int -> Int -> Int -> Int
        fib' count preFib prePreFib= 
            if n < 1 then
                0
            else if count >= n then
                preFib + prePreFib
            else
                fib' (count + 1) (preFib + prePreFib) preFib

-- reverse do
reverse :: ∀ a. Array a -> Array a
reverse = foldl (\acc a -> cons a acc) []

-- mock file system exercises
onlyFiles :: Path -> Array Path
onlyFiles (Directory _ children) = 
    concat [(filter (not isDirectory) children), (concatMap onlyFiles children)]
onlyFiles _ = []

-- find search a file
whereIs :: Path -> String -> Maybe Path
whereIs p fName = head $ do
    d <- allFiles p
    f <- ls d
    guard $ filename f == (filename d) <> fName
    pure d
-- whereIs p fName = head $ do
--     f <- allFiles p
--     guard $ containsFile fName f
--     pure f

containsFile :: String -> Path -> Boolean
containsFile fName (Directory p children) = maybeToBoolean $ head $ filter (matchFileName fName) children
containsFile _ _ = false

matchFileName :: String -> Path -> Boolean
matchFileName fName (File n _) = endsWith fName n
matchFileName _ _ = false

maybeToBoolean :: ∀ a. Maybe a -> Boolean
maybeToBoolean (Just _) = true
maybeToBoolean Nothing = false

-- small and large files of a path
largestSmallest :: Path -> Array Path
largestSmallest d@(Directory p children) = 
    let
        af = onlyFiles d
        largeOrder :: Path -> Path -> Ordering
        largeOrder (File _ s1) (File _ s2) = 
            if s1 > s2 then
                GT
            else if s1 == s2 then
                EQ
            else LT
        largeOrder _ _ = EQ
        smallOrder :: Path -> Path -> Ordering
        smallOrder p1 p2 = largeOrder p2 p1
        largest :: Array Path -> Path
        largest fs = fromMaybe (File "" 0) $head $ (sortBy largeOrder fs) 
        smallest :: Array Path -> Path
        smallest fs = fromMaybe (File "" 0) $head $ (sortBy smallOrder fs) 
    in
        if length af == 0 then
            []
        else if length af == 1 then
            af
        else
            [(largest af), (smallest af)]

largestSmallest _ = []