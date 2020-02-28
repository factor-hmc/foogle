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
    run port (mkApp db)

withDB :: (DB -> IO ()) -> IO ()
withDB f = do
  (eitherDecode <$> BL.readFile "db.json") >>= \case
    Left err  -> putStrLn err
    Right db -> f db 
