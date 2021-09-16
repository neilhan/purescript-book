module Test.MySolutions where

import Prelude

import Control.Monad.ST (for, run)
import Control.Monad.ST.Internal (modify, new, read, write)
import Data.Array (foldM, head, nub, sort, tail)
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.List (List(..), (:))
import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Exception (error, throwException)
import Math (pow)

-- Note to reader: Add your solutions to this file
third :: ∀ a. Array a -> Maybe a
third ary = do
    t2 <- tail ary
    t3 <- tail t2
    head t3

-- array of coins, int is their value. Returns possible values that can be done by combining these coins
possibleSums :: Array Int -> Array Int
possibleSums ary = nub $ sort $ foldM combine 0 ary
  where 
    combine :: Int -> Int -> Array Int
    combine acc c = [acc, acc + c] -- <> (map ((+) c) acc)
-- possibleSums xs = nub $ sort $ foldM (\acc i -> [ acc, acc + i ]) 0 xs

filterM :: ∀ m a. Monad m => (a -> m Boolean) -> List a -> m (List a)
filterM _ Nil = pure Nil
filterM f (x : xs) = do
  include <- f x
  restXs <- filterM f xs
  pure if include then (x : restXs) else restXs

-- exception
exceptionDivide :: Int -> Int -> Effect Int
exceptionDivide _ 0 = throwException $ error "div zero"
exceptionDivide a b = pure $ a / b

-- estimatePi - Gregory Series pi = 4 * Sum ((-1)^(k+1) / (2 * k - 1)) where k = 1 - n
estimatePi :: Int -> Number
estimatePi n = 
  run do
      ref <- new {acc: 0.0}
      for 1 (n+1) \i -> 
        let ni = toNumber i
        in
          modify (\o -> {acc: o.acc + (4.0 * (pow (-1.0) (ni+1.0)) / (2.0 * ni - 1.0))}) ref
      result <- read ref
      pure result.acc

estimatePi' :: Int -> Number
estimatePi' n = let
    nn = toNumber n
    est' acc n 
      | n == 0.0 = acc
      | otherwise = est' (acc + 4.0 * (pow (-1.0) (n+1.0)) / (2.0 * n - 1.0)) (n - 1.0)
  in
    est' 0.0 nn

-- fibonacci done with ST
fibonacci :: Int -> Int
fibonacci n = run do
  x <- new 0
  y <- new 1
  for 2 n \_ -> do
    x' <- read x
    y' <- read y
    _ <- write (x' + y') y
    write y' x
  x' <- read x
  y' <- read y
  pure (x' + y')