{-# LANGUAGE OverloadedStrings #-}
module Main where

import Parse
import Types
import Search
import Data.Foldable (traverse_)
import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
  loc <- getLine
  query <- T.getLine
  db  <- makeDB loc
  let ws = wordsMatchingVar query db
  traverse_ printModule ws

queryVar :: IO ()
queryVar = do
  db    <- readDB
  query <- T.getLine
  let ws = wordsMatchingVar query db
  traverse_ printModule ws

printModule :: (FilePath, [FactorWord]) -> IO ()
printModule (fp, ws) = do
  putStrLn $ fp <> ": "
  traverse_ (T.putStrLn . ("  " <>) . prettyWord) ws

createDB :: IO ()
createDB = do
  loc <- getLine
  db <- makeDB loc
  writeFile "db.txt" (show db)

readDB :: IO [(FilePath, [FactorWord])]
readDB = read <$> readFile "db.txt"
