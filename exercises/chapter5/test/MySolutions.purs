module Test.MySolutions where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Picture (Shape(..), Point, origin)


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

-- same city  returns true if 2 person is from the same city
-- sameCity :: ∀ (r1 :: Row Type) (r2 :: Row Type) (r3 :: Row Type) (r4 :: Row Type) (te :: Type). Eq te => {address::{city::te | r1} | r2} -> {address::{city::te | r3} | r4} -> Boolean
type HasAdrCity r1 r2 = {address::{city::String | r2} | r1}
sameCity :: ∀ r1 r2. HasAdrCity r1 r2 -> HasAdrCity r1 r2 -> Boolean
sameCity {address: {city: c1}} {address: {city: c2}} = c1 == c2

-- fromSingleton, if array has only one element, return it. Otherwise return default
fromSingleton :: ∀ a. a -> Array a -> a
fromSingleton dft ary = case ary of
    [a] -> a
    _ -> dft

-- circle
circleAtOrigin = Circle origin 10.0

-- double radius, center to origin
doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter = (scaleShape 2.0) >>> centerShape

scaleShape :: Number -> Shape -> Shape
scaleShape sc shape = case shape of
    (Circle { x, y } r) -> 
        Circle { x: (x*sc),y: (y*sc) } (r * sc)
    (Rectangle {x, y} n1 n2) -> 
        Rectangle { x: (x*sc),y: (y*sc) } (n1*sc) (n2 * sc)
    (Line {x:x1, y:y1} {x:x2, y:y2} ) -> 
        Line {x:x1*sc, y:y1*sc} {x:x2*sc, y:y2*sc} 
    (Text {x, y} str) -> 
        Text { x: (x*sc),y: (y*sc) } str

centerShape :: Shape -> Shape
centerShape shape = case shape of
    (Circle _ r)        -> Circle origin r
    (Rectangle _ n1 n2) -> Rectangle origin n1 n2
    (Text _ str)        -> Text origin str
    (Line {x:x1, y:y1} {x:x2, y:y2} ) -> 
        Line 
            {x:(x1-x2)/2.0 - origin.x, y:(y1-y2)/2.0 - origin.y} 
            {x:(x2-x1)/2.0 - origin.x, y:(y2-y1)/2.0 - origin.y}

-- extract text
shapeText :: Shape -> Maybe String
shapeText s = case s of
    (Text _ str) -> Just str
    _ -> Nothing

