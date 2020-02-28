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
{-# LANGUAGE ScopedTypeVariables #-}

module Server where

import Data.Aeson
import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.Maybe ( fromMaybe )
import Data.Proxy
import Data.Text ( Text )
import qualified Data.Text as T
import GHC.Generics

import Servant

import Parse
import Types
import Search
import Infer

type DB = [FactorWord Text]
type SearchResults = Headers '[Header "Access-Control-Allow-Origin" String] [FoogleResult]
type QueryAPI = QueryParam "search" Text :> QueryParam "numResults" Int :> Get '[JSON] SearchResults

data FoogleResult =
  FoogleResult
  { resultName   :: Text
  , resultEffect :: Effect Text
  }
  deriving (Eq, Show)

instance ToJSON FoogleResult where
  toJSON FoogleResult{..} = object [
    "name"   .= T.unpack resultName,
    "effect" .= show resultEffect ]

factorWordToResult :: FactorWord Text -> FoogleResult
factorWordToResult FactorWord{..} = FoogleResult
  { resultName   = wordName
  , resultEffect = fromMaybe emptyEffect wordEff
  }
  where
    emptyEffect = Effect [] [] False Nothing Nothing

queryAPI :: Proxy QueryAPI
queryAPI = Proxy

server :: DB -> Server QueryAPI
server db = search
  where
    search :: Maybe Text -> Maybe Int -> Handler SearchResults
    search _query (Just n)
      | n <= 0 = throwError $ err400 { errBody = "Number of results needs to be greater than 0." }
    -- eventually don't do this
    search Nothing _numResults = throwError $ err400 { errBody = "No search term provided." }
    search (Just query) numResults =
      case parseEffect query of
        Left err  -> throwError $ err400 { errBody = "Failed to parse input effect: " <> BLU.fromString err }
        Right eff -> return . addXOriginHeader . map factorWordToResult $
          searchDB (fromMaybe 5 numResults) eff db
    addXOriginHeader:: a -> Headers '[Header "Access-Control-Allow-Origin" String] a
    addXOriginHeader= addHeader "*"

mkApp :: DB -> Application
mkApp db = serve queryAPI (server db)
