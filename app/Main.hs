{-# LANGUAGE OverloadedStrings #-}
module Main where

import Parse
import Types
import Search
import Data.Aeson
import Data.Foldable (traverse_)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as BS
import Options.Applicative (execParser)

main :: IO ()
main = do
  query <- parseEffect <$> T.getLine
  case query of
    Left err  -> putStrLn err
    Right eff -> do
      db <- eitherDecode <$> BS.readFile "db.json"
      case db of
        Left err  -> putStrLn err
        Right db' -> traverse_ print $ searchDB 5 eff db'

-- run :: Query -> IO ()
-- run opts = do
--   db <- makeDB (fileName opts)
--   let ws = wordsMatchingQuery opts db
--   traverse_ printModule ws
-- 
-- printModule :: (FilePath, [FactorWord Highlighted]) -> IO ()
-- printModule (fp, ws) = do
--   putStrLn $ fp <> ": "
--   traverse_ (\w -> putStr "  " *> renderWord w *> putStr "\n") ws
-- 
-- queryVar :: IO ()
-- queryVar = do
--   db    <- readDB
--   query <- T.getLine
--   let ws = wordsMatchingVar query db
--   traverse_ printModule ws
-- 
-- 
-- createDB :: IO ()
-- createDB = do
--   loc <- getLine
--   db <- makeDB loc
--   writeFile "db.txt" (show db)
-- 
-- readDB :: IO [(FilePath, [FactorWord])]
-- readDB = read <$> readFile "db.txt"
