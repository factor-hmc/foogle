{-# LANGUAGE RecordWildCards #-}
module Search where

import Types
import Argparse
import Data.Text (Text)
import Data.List (isInfixOf)
import qualified Data.Text as T
import Control.Monad (guard)

effectInput' :: FactorWord -> [(Text, Maybe StackEffect)]
effectInput' (FactorWord _ (StackEffect ins _)) = ins

effectOutput' :: FactorWord -> [(Text, Maybe StackEffect)]
effectOutput' (FactorWord _ (StackEffect _ outs)) = outs

effectInput :: FactorWord -> [Text]
effectInput = map fst . effectInput'

effectOutput :: FactorWord -> [Text]
effectOutput = map fst . effectOutput'

getName :: FactorWord -> Text
getName (FactorWord n _) = n

-- The boolean indicates whether the match is strict
match :: Bool -> [Text] -> [Text] -> Bool
match True qs effs  = all (`elem` effs) qs
match False qs effs = all (\q -> any (q `T.isInfixOf`) effs) qs

wordsMatchingEffect 
  :: (FactorWord -> [Text])
  -> Bool 
  -> [Text] 
  -> Maybe Int 
  -> Maybe Int 
  -> [FactorWord] 
  -> [FactorWord]
wordsMatchingEffect getEffect strict qs minLen maxLen ws = do
  w <- ws
  let eff = getEffect w
  let len = length eff
  guard $ maybe (const True) (<=) minLen len
  guard $ maybe (const True) (>=) maxLen len
  guard $ match strict qs eff
  pure w

wordsMatchingInEffect = wordsMatchingEffect effectInput
wordsMatchingOutEffect = wordsMatchingEffect effectOutput

wordsMatchingName :: Bool -> Text -> [FactorWord] -> [FactorWord]
wordsMatchingName True name = filter ((name ==) . getName)
wordsMatchingName False name = filter ((name `T.isInfixOf`) . getName)

wordsMatchingQuery 
  :: Options 
  -> [(FilePath, [FactorWord])] 
  -> [(FilePath, [FactorWord])]
wordsMatchingQuery Options{..} = filter (not . null . snd) . map wordsMatchingQuery'
  where
    wordsMatchingQuery' (fp, ws) = (fp, 
        wordsMatchingInEffect strict inQueries inMin inMax
      . wordsMatchingOutEffect strict outQueries outMin outMax
      . maybe id (wordsMatchingName strict) wordName $
      ws)
