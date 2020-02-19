{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}

module Server where

import Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics

import Servant

import Parse
import Types
import Search
import Infer

type DB = [FactorWord Text]
type QueryAPI = QueryParam "search" Text :> QueryParam "numResults" Int :> Get '[JSON] [FoogleResult]

data FoogleResult =
  FoogleResult
  { resultName   :: String
  , resultEffect :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON FoogleResult

factorWordToResult :: FactorWord Text -> FoogleResult
factorWordToResult FactorWord{..} = FoogleResult
  { resultName   = T.unpack wordName
  , resultEffect = maybe "" show $ wordEff
  }

queryAPI :: Proxy QueryAPI
queryAPI = Proxy

server :: DB -> Server QueryAPI
server db = search
  where
    search :: Maybe Text -> Maybe Int -> Handler [FoogleResult]
    search _query (Just n)
      | n <= 0 = throwError $ err400 { errBody = "Number of results needs to be greater than 0." }
    -- eventually don't do this
    search Nothing _numResults = throwError $ err400 { errBody = "No search term provided." }
    search (Just query) numResults =
      case parseEffect query of
        Left err  -> throwError $ err400 { errBody = "Failed to parse input effect: " <> BLU.fromString err }
        Right eff -> return . map factorWordToResult $
          searchDB (fromMaybe 5 numResults) eff db

mkApp :: DB -> Application
mkApp db = serve queryAPI (server db)
