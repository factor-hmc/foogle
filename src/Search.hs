{-# LANGUAGE RecordWildCards #-}
module Search where

import Data.List (sortOn)
import Data.Text(Text(..))
import qualified Data.Text as T

import Utils
import Types

effectEditDistanceOptions :: EditDistanceOptions (EffVar Text)
effectEditDistanceOptions = EditDistanceOptions
  { insertCost = const 2
  , deleteCost = const 2
  -- This probably should recursively compare effects (e.g. for quotations)
  , replaceCost = const (const 1)
  }

compareEffVars :: [EffVar Text] -> [EffVar Text] -> Int
compareEffVars = editDistance effectEditDistanceOptions 

cost :: Effect Text -> Effect Text -> Int
cost eff1 eff2 = 
  compareEffVars (effIn eff1) (effIn eff2)
  + compareEffVars (effOut eff1) (effOut eff2)

searchDB :: Int -> Effect Text -> [FactorWord Text] -> [FactorWord Text]
searchDB numResults query db = take numResults $ sortOn calculateCost db
  where
    calculateCost FactorWord{..} = case wordEff of
      -- absurdly high costs for words without effects
      Nothing  -> 1000
      Just eff -> cost query eff
