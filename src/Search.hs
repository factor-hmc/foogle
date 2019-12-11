{-# LANGUAGE RecordWildCards #-}
module Search where

import Types
import Argparse

import Data.Text (Text)
import Data.List (isInfixOf)
import qualified Data.Text as T
import qualified Data.Text.Internal.Search as T
import Data.Maybe
import Control.Monad (guard, foldM)

effectInput' :: SimpleWord -> [(Text, Maybe SimpleEffect)]
effectInput' (FactorWord _ (StackEffect ins _)) = ins

effectOutput' :: SimpleWord -> [(Text, Maybe SimpleEffect)]
effectOutput' (FactorWord _ (StackEffect _ outs)) = outs

effectInput :: SimpleWord -> [Text]
effectInput = map fst . effectInput'

effectOutput :: SimpleWord -> [Text]
effectOutput = map fst . effectOutput'

getName :: SimpleWord -> Text
getName (FactorWord n _) = n

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

matchHighlighted :: Bool -> Text -> Highlighted -> Maybe Highlighted
matchHighlighted strict needle (Highlighted haystack highlights) = do
  highlights' <- match strict needle haystack
  Just $ Highlighted haystack (highlights <> highlights')

matchBase :: Bool -> Text -> Text -> Maybe Highlighted
matchBase strict needle haystack = Highlighted haystack <$> match strict needle haystack

--matchNeedles :: Bool -> [Text] -> Text -> Maybe Highlighted
--matchNeedles strict needles haystack = Highlighted haystack <$> highlights
--  where
--    highlights = foldMap (\needle -> match strict haystack needle) needles

matchQuery :: Query -> SimpleWord -> Maybe (FactorWord Highlighted)
matchQuery Query{..} (FactorWord name stackEffect) = do
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
    seIn = StackEffect (getInEffects stackEffect) []
    seOut = StackEffect [] (getOutEffects stackEffect)
    matchSE :: SimpleEffect -> [Text] -> Maybe (StackEffect Highlights)
    matchSE se [] = Just $ Highlights [] <$ se
    matchSE se queries = do
      let matchedQs = [match strict q <$> se | q <- queries]
      guard $ all (any isJust) matchedQs
      fromMaybeSE $ foldr1 (\se1 se2 -> (<>) <$> se1 <*> se2) matchedQs

wordsMatchingQuery :: Query -> [(FilePath, [SimpleWord])] -> [(FilePath, [FactorWord Highlighted])]
wordsMatchingQuery q db = do 
  (fp,ws) <- db
  let matching = catMaybes $ map (matchQuery q) ws
  guard $ not (null matching)
  pure (fp, matching)
      
--matchQuery Query{..} word = case (inQueries, outQueries) of
--  (inQ:inQs, outQs) -> do
--    let start = matchBase strict inQ <$> word
--    guard (any isJust start)
--  ([], outQ:outQs)  -> undefined
--  ([], []) -> Just $ fmap (flip Highlighted mempty) word

--wordsMatchingEffect 
--  :: (FactorWord -> [Text])
--  -> Bool 
--  -> [Text] 
--  -> Maybe Int 
--  -> Maybe Int 
--  -> [FactorWord] 
--  -> [FactorWord]
--wordsMatchingEffect getEffect strict qs minLen maxLen ws = do
--  w <- ws
--  let eff = getEffect w
--  let len = length eff
--  guard $ maybe (const True) (<=) minLen len
--  guard $ maybe (const True) (>=) maxLen len
--  guard $ match strict qs eff
--  pure w
--
--wordsMatchingInEffect = wordsMatchingEffect effectInput
--wordsMatchingOutEffect = wordsMatchingEffect effectOutput
--
--wordsMatchingName :: Bool -> Text -> [FactorWord] -> [FactorWord]
--wordsMatchingName True name = filter ((name ==) . getName)
--wordsMatchingName False name = filter ((name `T.isInfixOf`) . getName)
--
--wordsMatchingQuery 
--  :: Query
--  -> [(FilePath, [FactorWord])] 
--  -> [(FilePath, [FactorWord])]
--wordsMatchingQuery Query{..} = filter (not . null . snd) . map wordsMatchingQuery'
--  where
--    wordsMatchingQuery' (fp, ws) = (fp, 
--        wordsMatchingInEffect strict inQueries inMin inMax
--      . wordsMatchingOutEffect strict outQueries outMin outMax
--      . maybe id (wordsMatchingName strict) wordName $
--      ws)
