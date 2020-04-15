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
  { insertCost = const 4
  , deleteCost = const 4
  -- This probably should recursively compare effects (e.g. for quotations)
  , replaceCost = effVarCost
  }

effVarCost :: EffVar Text -> EffVar Text -> Int
effVarCost queryV effV= case (queryV, effV) of
  (EffVar qv, EffVar ev)
           | qv == ev                      -> -1
           | qv `T.isInfixOf` ev           -> 0
           | otherwise                     -> 2
  (TypedEffVar qv [qtp], TypedEffVar ev etps)
           | qv == ev && qtp `elem` etps   -> -2
           | qtp `elem` etps               -> -1
           | qv == ev                      -> 0
           | qv `T.isInfixOf` ev           -> 1
           | otherwise                     -> 2
  -- Eventually, we may add support for this, but it's better to explicitly fail for now
  (TypedEffVar qv _, TypedEffVar _ _) -> 
    error "query effvar has more than one type (this shouldn't happen)"
  (QuotEffVar qv qeff, QuotEffVar ev eeff) ->
    let c = cost qeff eeff
     in if | qv == ev && c <= 0            -> c - 2
           | qv `T.isInfixOf` ev && c <= 0 -> c - 1
           | qv == ev                      -> 0
           | qv `T.isInfixOf` ev           -> 1
           | otherwise                     -> 2
  _ -> 
    let qName = effVarName queryV
        eName = effVarName effV
     in if | qName == eName                -> 0
           | qName `T.isInfixOf` eName     -> 1
           | otherwise                     -> 2

compareEffVars :: [EffVar Text] -> [EffVar Text] -> Int
compareEffVars = editDistance effectEditDistanceOptions 

cost :: Effect Text -> Effect Text -> Int
cost queryEff eff = 
  compareEffVars (effIn queryEff) (effIn eff)
  + compareEffVars (effOut queryEff) (effOut eff)

searchDB :: Int -> Effect Text -> [FactorWord Text] -> [FactorWord Text]
searchDB numResults query db = take numResults $ sortOn calculateCost db
  where
    calculateCost FactorWord{..} = case wordEff of
      -- absurdly high costs for words without effects
      Nothing  -> 1000
      Just eff -> cost query eff
