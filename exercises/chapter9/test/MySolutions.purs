module Test.MySolutions where

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as ResponseFormat
import Control.Parallel (parTraverse, parallel, sequential)
import Data.Array (concat, cons, filter, (:))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), length, split)
import Data.Traversable (fold, oneOf, traverse)
import Effect.Aff (Aff, Milliseconds(..), attempt, delay)
import Effect.Console (log)
import Effect.Exception (Error)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Node.Path (FilePath)
import Node.Path as Path

-- Note to reader: Add your solutions to this file

concatenateFiles :: FilePath -> FilePath -> FilePath -> Aff Unit
concatenateFiles in1 in2 outFile = do
  content1 <- readTextFile UTF8 in1
  content2 <- readTextFile UTF8 in2
  writeTextFile UTF8 outFile (content1 <> content2)

concatenateMany :: Array FilePath -> FilePath -> Aff Unit
concatenateMany files outFile = do
  contents <- traverse (readTextFile UTF8) files
  concatContent <- pure (fold contents)
  writeTextFile UTF8 outFile concatContent

countCharacters :: FilePath -> Aff (Either Error Int)
countCharacters file = do
  result <- attempt $ readTextFile UTF8 file
  pure $ case result of
            Right content -> Right (length content)
            Left e -> Left e

-- writeGet, read url, and write to file
writeGet :: String -> FilePath -> Aff Unit
writeGet url outFile = do
  result <- AX.get ResponseFormat.string url
  case result of
    Left e -> pure unit -- "GET " <> url <> " failed: " <> AX.printError e
    Right resp -> writeTextFile UTF8 outFile resp.body

concatenateManyParallel :: Array FilePath -> FilePath -> Aff Unit
concatenateManyParallel files outFile = do
  contents <- parTraverse (readTextFile UTF8) files
  concatContent <- pure (fold contents)
  writeTextFile UTF8 outFile concatContent

-- getWithTimeout - GET request, returns nothing if timeout. Or string response if succeed
getWithTimeout :: Number -> String -> Aff (Maybe String)
getWithTimeout mil url = 
  sequential $ oneOf
    [ parallel ((do 
                    result <- AX.get ResponseFormat.string url
                    pure case result of
                        Left _ -> Nothing -- "error: " <> AX.printError e
                        Right resp -> Just resp.body))
    , parallel (Nothing <$ delay (Milliseconds mil))]

-- recursive read file
recurseFiles :: FilePath -> Aff (Array String)
recurseFiles file = do
  content <- readTextFile UTF8 file
  let 
    dir = Path.dirname $ Path.normalize file
    filePaths = (map (\p -> Path.concat [dir, p]) 
                    (Path.normalize <$> 
                    filter (\p -> p /= "") (split (Pattern "\n") content)))
  recFilePathsArrays <- parTraverse recurseFiles filePaths -- traverse 
  let recFilePaths = concat recFilePathsArrays
  pure ( file : recFilePaths)
