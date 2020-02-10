{-# LANGUAGE RecordWildCards #-}
module Utils where

import Data.Array

-- | Configurable costs for the edit distance function.
-- These can be adjusted based on the elements themselves.
data EditDistanceOptions a = EditDistanceOptions
  { insertCost  :: a -> Int      -- ^ Cost to insert an element
  , deleteCost  :: a -> Int      -- ^ Cost to delete an element
  , replaceCost :: a -> a -> Int -- ^ Cost to replace one element by another
  }

defaultEditDistanceOptions :: EditDistanceOptions a
defaultEditDistanceOptions = EditDistanceOptions
  { insertCost  = const 1
  , deleteCost  = const 1
  , replaceCost = const (const 1)
  }

-- | DP version of edit distance taken from https://wiki.haskell.org/Edit_distance.
-- Modified slightly to give more options.
editDistance :: Eq a => EditDistanceOptions a -> [a] -> [a] -> Int
editDistance EditDistanceOptions{..} xs ys = table ! (m,n)
    where
    (m,n) = (length xs, length ys)
    x     = array (1,m) (zip [1..] xs)
    y     = array (1,n) (zip [1..] ys)
    
    table :: Array (Int,Int) Int
    table = array bnds [(ij, dist ij) | ij <- range bnds]
    bnds  = ((0,0),(m,n))
    
    dist (0,j) = j
    dist (i,0) = i
    dist (i,j) = minimum 
      [ table ! (i-1,j) + deleteCost elem1
      , table ! (i,j-1) + insertCost elem2
      ,  if elem1 == elem2
           then table ! (i-1,j-1) 
           else replaceCost elem1 elem2 + table ! (i-1,j-1)
      ]
      where
        elem1 = x ! i
        elem2 = y ! j
