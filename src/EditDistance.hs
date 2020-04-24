{-# LANGUAGE RecordWildCards #-}
module EditDistance where

import Data.Array

-- | Configurable costs for the edit distance function.
-- These can be adjusted based on the elements themselves.
-- 
-- The insert/delete costs are constants so that calculating the
-- edit distance is slightly faster, but it's simple to change
-- to make it depend on the element itself.
data EditDistanceOptions a = EditDistanceOptions
  { insertCost  :: Int           -- ^ Cost to insert an element
  , deleteCost  :: Int           -- ^ Cost to delete an element
  , replaceCost :: a -> a -> Int -- ^ Cost to replace one element by another
  }

defaultEditDistanceOptions :: Eq a => EditDistanceOptions a
defaultEditDistanceOptions = EditDistanceOptions
  { insertCost  = 1
  , deleteCost  = 1
  , replaceCost = \x -> \y -> 
      if x == y 
        then 0
        else 1
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
    
                                -- If the cost is a function, we can use the below
    dist (0,j) = insertCost * j -- sum [ insertCost (y ! yy) | yy <- [1..j] ]
    dist (i,0) = deleteCost * i -- sum [ deleteCost (x ! xx) | xx <- [1..i] ]
    dist (i,j) = minimum 
      [ table ! (i-1,j) + deleteCost -- elem1
      , table ! (i,j-1) + insertCost -- elem2
      , replaceCost elem1 elem2 + table ! (i-1,j-1)
      ]
      where
        elem1 = x ! i
        elem2 = y ! j
