{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiWayIf #-}
module Search where

import Data.List (sortOn)
import Data.Text(Text(..))
import qualified Data.Text as T

import EditDistance
import Types

effectEditDistanceOptions :: EditDistanceOptions (EffVar Text)
effectEditDistanceOptions = EditDistanceOptions
  { insertCost = 3
  , deleteCost = 3
  , replaceCost = effVarCost
  }

effVarCost :: EffVar Text -> EffVar Text -> Int
effVarCost queryV effV= case (queryV, effV) of
  (EffVar qv, EffVar ev) ->
    let nameMatch = qv == ev
        nameClose = qv `T.isInfixOf` ev
     in if | nameMatch             -> -1
           | nameClose             -> 0
           | otherwise             -> 2
  (TypedEffVar qv qtps, TypedEffVar ev etps) ->
        -- If we're searching with a union, only one of them needs to match
    let tpMatch   = any (`elem` etps) qtps
        nameMatch = qv == ev
        nameClose = qv `T.isInfixOf` ev
     in if | nameClose && tpMatch -> -3
           | tpMatch              -> -2
           | nameMatch            -> 0
           | nameClose            -> 1
           | otherwise            -> 2
  (QuotEffVar qv qeff, QuotEffVar ev eeff) ->
    let c = cost qeff eeff
        nameMatch = qv == ev
        nameClose = qv `T.isInfixOf` ev
     in if | nameClose && c <= 0  -> c - 3
           | c <= 0               -> c - 2
           | nameMatch            -> 0
           | nameClose            -> 1
           | otherwise            -> 2
     
  -- EVar mismatches we'll usually treat as problems, but if the names are the same
  -- we'll say they cost nothing.
  _ -> 
    let qName = effVarName queryV
        eName = effVarName effV
        nameMatch = qName == eName
        nameClose = qName `T.isInfixOf` eName
     in if | nameMatch            -> 0
           | nameClose            -> 1
           | otherwise            -> 2

compareEffVars :: [EffVar Text] -> [EffVar Text] -> Int
compareEffVars = editDistance effectEditDistanceOptions 

cost :: Effect Text -> Effect Text -> Int
cost queryEff eff = 
  compareEffVars (effIn queryEff) (effIn eff)
  + compareEffVars (effOut queryEff) (effOut eff)

searchDB :: Int -> Effect Text -> [FactorWord Text] -> [FactorWord Text]
searchDB numResults query db = take numResults $ sortOn calculateCost db -- sortOn fst . map ((,) <$> calculateCost <*> id) $ db
  where
    calculateCost FactorWord{..} = case wordEff of
      -- absurdly high costs for words without effects
      Nothing  -> 1000
      Just eff -> cost query eff
