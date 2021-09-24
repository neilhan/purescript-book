module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (Aff, launchAff, launchAff_)
import Effect.Class.Console (log)

-- -- main :: Effect Unit
-- -- main = pure unit

-- -- main :: Effect Int
-- -- main = do
-- --   log "hello"
-- --   pure 100

main :: Effect Unit
main = 
    launchAff_ do
        log $ show 10

-- import Prelude
-- import Data.Either (Either(..))
-- import Effect (Effect)
-- import Effect.Aff (Aff, attempt, launchAff_, message)
-- import Effect.Class.Console (log)
-- import Node.Encoding (Encoding(..))
-- import Node.FS.Aff (readTextFile, writeTextFile)
-- import Node.Path (FilePath)

-- copyFile :: FilePath -> FilePath -> Aff Unit
-- copyFile file1 file2 = do
--   my_data <- readTextFile UTF8 file1
--   writeTextFile UTF8 file2 my_data

-- main :: Effect Unit
-- main = launchAff_ do
--   result <- attempt $ copyFile "file1.txt" "file2.txt"
--   case result of
--     Left e -> log $ "There was a problem with copyFile: " <> message e
--     _ -> pure unit