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
