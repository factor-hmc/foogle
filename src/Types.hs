{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ApplicativeDo #-}
module Types where

import Data.Text (Text(..))
import qualified Data.Text as T

data FactorWord a
  = FactorWord
      a              -- ^ The name of the word
      (StackEffect a)-- ^ The stack effect of the word
  deriving (Read, Show, Eq, Functor)

data StackEffect a
  = StackEffect 
      [(a, Maybe (StackEffect a))] -- ^ The input stack variables
      [(a, Maybe (StackEffect a))] -- ^ The output stack variables
  deriving (Read, Show, Eq, Functor)

getInEffects :: StackEffect a -> [(a, Maybe (StackEffect a))]
getInEffects (StackEffect ins _) = ins

getOutEffects :: StackEffect a -> [(a, Maybe (StackEffect a))]
getOutEffects (StackEffect _ outs) = outs

instance Foldable FactorWord where
  foldMap f (FactorWord name se) = foldMap f se

instance Foldable StackEffect where
  foldMap f (StackEffect ins outs) = foldMap foldTuple ins <> foldMap foldTuple outs
    where
      foldTuple (a, Nothing) = f a
      foldTuple (a, Just rc) = f a <> foldMap f rc

instance Traversable StackEffect where
  sequenceA (StackEffect ins outs) = do
    ins'  <- sequenceA' ins
    outs' <- sequenceA' outs
    pure $ StackEffect ins' outs'
      where
        sequenceA' [] = pure []
        sequenceA' ((a, Nothing):es) = do
          a'  <- a
          es' <- sequenceA' es
          pure $ (a', Nothing) : es'
        sequenceA' ((a, Just eff):es) = do
          a'  <- a
          es' <- sequenceA' es
          e' <- sequenceA eff
          pure $ (a', Just e') : es'

instance Applicative StackEffect where
  pure x = StackEffect (repeat (x, Just (pure x))) (repeat (x, Just (pure x)))
  (StackEffect fs gs) <*> (StackEffect as bs) = 
    StackEffect (ap fs as) (ap gs bs)
      where
        ap funcs list = zipWith combine funcs list
        combine func val = case (func, val) of
          ((f, Just fSE), (a, Just aSE)) -> (f a, Just $ fSE <*> aSE)
          ((f, _), (a, _))               -> (f a, Nothing)

type SimpleWord   = FactorWord Text
type SimpleEffect = StackEffect Text

type HighlightedWord = FactorWord Highlighted
type HighlightedEffect= StackEffect Highlighted

-- | Datatype for output to terminal
data Highlighted
  = Highlighted 
      Text        -- ^ The highlighted text
      Highlights  -- ^ Locations of the highlights
  deriving (Show, Eq)

-- | Locations of the highlights in a string, in order
newtype Highlights
  = Highlights { getHighlights :: [(Int, Int)]}
  deriving (Show, Eq)

instance Semigroup Highlights where
  (Highlights []) <> (Highlights h2s) = Highlights h2s
  (Highlights h1s) <> (Highlights []) = Highlights h1s
  (Highlights ((x1,y1):h1s)) <> (Highlights ((x2,y2):h2s)) =
       -- The two highlights intersect and can be combined
    if | intersects x1 y1 x2 y2 -> Highlights ((min x1 x2, max y1 y2):h1s) <> Highlights h2s
       -- (x1, y1) is strictly behind (x2, y2)
       | y1 < x2                -> Highlights $ (x1, y1) : (x2, y2) : getHighlights (Highlights h1s <> Highlights h2s)
       -- (x2, y2) is strictly behind (x1, y1)
       | otherwise              -> Highlights $ (x2, y2) : (x1, y1) : getHighlights (Highlights h1s <> Highlights h2s)
    where
      intersects x1 y1 x2 y2 = x1 <= y2 && x2 <= y1 || x2 <= y1 && x1 <= y2

instance Monoid Highlights where
  mempty  = Highlights []
  mappend = (<>)

data Query
  = Query
  { inMin      :: Maybe Int -- ^ The minimum number of elements in the input of a stack effect
  , inMax      :: Maybe Int -- ^ The maximum number of elements in the input of a stack effect
  , inQueries  :: [Text] -- ^ The queries for the input stack effect (unordered)
  , outMin     :: Maybe Int -- ^ The minimum number of elements in the output of a stack effect
  , outMax     :: Maybe Int -- ^ The maximum number of elements in the output of a stack effect
  , outQueries :: [Text] -- ^ The queries for the output stack effect (unordered)
  , wordName   :: Maybe Text -- ^ The name of the word
  , strict     :: Bool -- ^ Strictly match the queries?
  , fileName   :: FilePath -- ^ Name of the file to index
  }
