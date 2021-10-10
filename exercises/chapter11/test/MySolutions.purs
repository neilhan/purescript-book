module Test.MySolutions where

import Prelude

import Control.Monad.Reader (Reader, ask, local, runReader)
import Control.Monad.State (State, evalState, execState, get, gets, modify, modify_)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Array (filter)
import Data.Foldable (traverse_)
import Data.Monoid (power)
import Data.Monoid.Additive (Additive(..))
import Data.String (joinWith)
import Data.String.CodeUnits (toCharArray)
import Data.Traversable (sequence, traverse)
import Data.Tuple (Tuple)

-- Note to reader : Add your solutions to this file

-- testParens
-- countParens returns positive, zero, or negative counting of ( or ). 
-- ( + 1. ) -1
-- if counting state value is less than 0, don't count ( anymore.
countParens :: Array Char -> State Int Unit
countParens = 
  traverse_ 
    \c -> modify 
      \count -> case c of
        '(' -> if count < 0 then count
                else count + 1 
        ')' -> if count< 0 then count
                else count - 1
        _ -> count

testParens :: String -> Boolean
testParens s = 
    case (execState (countParens $ filter (\c -> (c == '(' || c == ')') ) (toCharArray s)) 0) of
        0 -> true
        _ -> false

-- Reader exercises --
type Level = Int
type Doc = (Reader Level) String

line :: String -> Doc
line str = do
  level <- ask
  pure $ (power "  " level) <> str

indent :: Doc -> Doc
indent = local \l -> l + 1

cat :: Array Doc -> Doc
cat = sequence >=> joinWith "\n" >>> pure

render :: Doc -> String
render r = runReader r 0

-- writer exercises
sumArrayWriter :: Array Int -> Writer (Additive Int) Unit 
sumArrayWriter = traverse_ \i -> do 
                    tell $ Additive i
                    pure unit

collatz :: Int -> Tuple Int (Array Int)
collatz nn = runWriter (col 0 nn)
  where 
    col :: Int -> Int -> Writer (Array Int) Int
    col c 1 = do
                tell [1] 
                pure c
    col c n = do
        tell [n]
        if (mod n 2 == 0) then 
            col (c + 1) (n / 2)
        else
            col (c  + 1) (n * 3 + 1)
