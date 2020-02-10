{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Parse
import Types
import Search
import Infer
import Argparse

import Data.Aeson
import Data.Foldable (traverse_)
import Data.Text (Text(..))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as BS
import Options.Applicative (execParser)

main :: IO ()
main = execParser optParser >>= run

run :: Query -> IO ()
run Query{..} = withDB $ \db ->
  let inferredDB = map infer db
  in traverse_ print $ searchDB queryNumResults queryStackEffect inferredDB

withDB :: ([FactorWord Text] -> IO ()) -> IO ()
withDB f = do
  (eitherDecode <$> BS.readFile "db.json") >>= \case
    Left err  -> putStrLn err
    Right db -> f db 

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
