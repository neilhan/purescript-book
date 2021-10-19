module Test.MySolutions where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT, lift)
import Control.Monad.Reader (Reader, ReaderT, ask, local, runReader, runReaderT)
import Control.Monad.State (State, StateT, evalState, execState, get, gets, modify, modify_, put)
import Control.Monad.Writer (Writer, WriterT, execWriterT, runWriter, tell)
import Data.Array (filter, fold, some)
import Data.Foldable (traverse_)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Monoid (power)
import Data.Monoid.Additive (Additive(..))
import Data.Newtype (unwrap)
import Data.String (Pattern(..), joinWith, stripPrefix)
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

-- ---------------------
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

-- ---------------------
type Doc' = (WriterT (Array String) (ReaderT Level Identity)) Unit

line' :: String -> Doc'
line' str = do
  level <- lift $ ask -- TODO try without $
  tell [(power "  " level) <> str]
  pure unit

indent' :: Doc' -> Doc'
indent' = local $ (+) 1

render' :: Doc' -> String
render' doct = joinWith "\n" $ unwrap $ runReaderT (execWriterT doct) 0

-- ---------------------
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

-- ExceptT
safeDivide :: Int -> Int -> ExceptT String Identity Int
safeDivide _ 0 = throwError "Divide by zero!"
safeDivide a b = do pure $ a / b

type Errors = Array String
type Log = Array String
type Parser = StateT String (WriterT Log (ExceptT Errors Identity))
-- string :: String -> Parser String
-- string p = do
--   s <- get
--   lift $ tell ["The state is " <> s]
--   case stripPrefix (Pattern p) s of
--     Just r -> do 
--         put r
--         pure p
--     Nothing -> lift $ lift $ throwError ["Could not parse"]


string :: String -> Parser String
string p = do
  s <- get
  tell ["The state is " <> s]
  case stripPrefix (Pattern p) s of
    Just r -> do 
        put r
        pure p
    Nothing -> throwError ["Could not parse"]

asFollowedByBs :: Parser String
asFollowedByBs = do
  a_s <- some $ string "a"
  b_s <- some $ string "b"
  pure $ fold $ a_s <> b_s

asOrBs :: Parser String
asOrBs = fold <$> some (string "a" <|> string "b")