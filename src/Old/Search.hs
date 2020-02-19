{-# LANGUAGE RecordWildCards #-}
module Old.Search where

import Old.Types
import Old.Argparse

import Data.Text (Text)
import Data.List (isInfixOf)
import qualified Data.Text as T
import qualified Data.Text.Internal.Search as T
import Data.Maybe
import Control.Monad (guard, foldM)

-- The boolean indicates whether the match is strict
match :: Bool -> Text -> Text -> Maybe Highlights
match True needle haystack
  | needle == haystack = Just $ Highlights [(0, T.length haystack - 1)]
  | otherwise = Nothing
match False needle haystack =
  if null matches
    then Nothing
    else Just $ Highlights matches
  where
    len     = T.length needle - 1
    matches = (,) <*> (+ len) <$> T.indices needle haystack

matchQuery :: Query -> SimpleWord -> Maybe (FactorWord Highlighted)
matchQuery Query{..} (FactorWord name stackEffect) = do
  guard $ maybe True (insLength >=) inMin
  guard $ maybe True (insLength <=) inMax
  guard $ maybe True (outsLength >=) outMin
  guard $ maybe True (outsLength <=) outMax
  matchedIn  <- matchSE seIn inQueries
  matchedOut <- matchSE seOut outQueries
  let newIns  = getInEffects (Highlighted <$> seIn <*> matchedIn)
      newOuts = getOutEffects (Highlighted <$> seOut <*> matchedOut)
      newSE   = StackEffect newIns newOuts
  matchedName <- case wordName of
                  Nothing    -> pure $ Highlighted name mempty
                  Just wordQ -> do
                    nameHighlights <- match strict wordQ name
                    pure $ Highlighted name nameHighlights
  pure $ FactorWord matchedName newSE
  where
    insLength  = length . getInEffects $ stackEffect
    outsLength = length . getOutEffects $ stackEffect
    seIn = StackEffect (getInEffects stackEffect) []
    seOut = StackEffect [] (getOutEffects stackEffect)
    matchSE :: SimpleEffect -> [Text] -> Maybe (StackEffect Highlights)
    matchSE se [] = Just $ Highlights [] <$ se
    matchSE se queries = do
      let matchedQs = [match strict q <$> se | q <- queries]
      guard $ all (any isJust) matchedQs
      sequence $ foldr1 (\se1 se2 -> (<>) <$> se1 <*> se2) matchedQs

wordsMatchingQuery :: Query -> [(FilePath, [SimpleWord])] -> [(FilePath, [FactorWord Highlighted])]
wordsMatchingQuery q db = do 
  (fp,ws) <- db
  let matching = catMaybes $ map (matchQuery q) ws
  guard $ not (null matching)
  pure (fp, matching)