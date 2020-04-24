{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Aeson ( eitherDecode )
import qualified Data.ByteString.Lazy as BL
import System.Environment ( getEnv ) 

import Server ( DB, mkApp )

-- import Network.Wai
import Network.Wai.Handler.Warp

main :: IO ()
main = do
  port <- read <$> getEnv "PORT"
  withDB $ \db -> 
    let inferredDB = map infer db
     in run port (mkApp inferredDB)

withDB :: (DB -> IO ()) -> IO ()
withDB f = do
  (eitherDecode <$> BL.readFile "db.json") >>= \case
    Left err  -> putStrLn err
    Right db -> f db 
