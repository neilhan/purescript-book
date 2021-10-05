module Test.MySolutions where

import Prelude

import Control.Monad.State (State, evalState, execState, modify)
import Data.Array (filter)
import Data.Foldable (traverse_)
import Data.String.CodeUnits (toCharArray)

-- Note to reader : Add your solutions to this file

-- testParens
-- countParens returns positive, zero, or negative counting of ( or ). 
-- ( + 1. ) -1
-- if counting state value is less than 0, don't count ( anymore.
countParens :: Array Char -> State Int Unit
countParens = traverse_ \c -> modify \count -> case c of
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