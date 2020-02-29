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
import qualified Data.ByteString.Lazy as BL
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

maxResults :: Int
maxResults = 500

type DB = [FactorWord Text]
type SearchResults = Headers '[Header "Access-Control-Allow-Origin" String] [FoogleResult]
type QueryAPI = QueryParam "search" Text :> QueryParam "numResults" Int :> Get '[JSON] SearchResults

data FoogleResult =
  FoogleResult
  { resultName     :: Text
  , resultEffect   :: Effect Text
  , resultURL      :: Text
  , resultVocab    :: Text
  , resultVocabURL :: Text
  }
  deriving (Eq, Show)

instance ToJSON FoogleResult where
  toJSON FoogleResult{..} = object 
   [ "name"           .= T.unpack resultName
   , "effect"         .= show resultEffect 
   , "url"            .= T.unpack resultURL 
   , "vocabulary"     .= T.unpack resultVocab
   , "vocabulary_url" .= T.unpack resultVocabURL
   ]

factorWordToResult :: FactorWord Text -> FoogleResult
factorWordToResult FactorWord{..} = FoogleResult
  { resultName     = wordName
  , resultEffect   = fromMaybe emptyEffect wordEff
  , resultURL      = wordURL
  , resultVocab    = wordVocab
  , resultVocabURL = wordVocabURL
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
      | n <= 0 = throwError $ err400 { errBody    = "Number of results needs to be greater than 0." 
                                     , errHeaders = corsHeader }
      | n > maxResults = throwError $ err400 { errBody = showBL n <> " is too many requested ( " <> showBL maxResults <> " is the maximum)"
                                             , errHeaders = corsHeader }
    -- eventually don't do this
    search Nothing _numResults = throwError $ err400 { errBody = "No search term provided." 
                                                     , errHeaders = corsHeader }
    search (Just query) numResults =
      case parseEffect query of
        Left err  -> throwError $ err400 { errBody = "Failed to parse input effect: " <> BLU.fromString err 
                                         , errHeaders = corsHeader }
        Right eff -> return . addCORSHeader . map factorWordToResult $
          searchDB (fromMaybe 5 numResults) eff db

    addCORSHeader:: a -> Headers '[Header "Access-Control-Allow-Origin" String] a
    addCORSHeader = addHeader "*"

    -- the plain CORS headers generated from 'addCORSHeader'
    corsHeader = getHeaders $ addCORSHeader undefined

mkApp :: DB -> Application
mkApp db = serve queryAPI (server db)

showBL :: Show a => a -> BL.ByteString
showBL = BLU.fromString . show
