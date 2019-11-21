module Search where

import Types
import Data.Text (Text)
import qualified Data.Text as T

effectInput :: FactorWord -> [(Text, Maybe StackEffect)]
effectInput (FactorWord _ (StackEffect ins _)) = ins

effectOutput :: FactorWord -> [(Text, Maybe StackEffect)]
effectOutput (FactorWord _ (StackEffect _ outs)) = outs

inEffectInput :: Text -> [FactorWord] -> [FactorWord]
inEffectInput v = filter (any ((== v) . fst) . effectInput)

inEffectOutput :: Text -> [FactorWord] -> [FactorWord]
inEffectOutput v = filter (any ((== v) . fst) . effectOutput)

wordsMatchingVar :: Text -> [(FilePath, [FactorWord])] -> [(FilePath, [FactorWord])]
wordsMatchingVar v = filter (not . null . snd) . map wordsMatchingVar'
  where
    wordsMatchingVar' (fp, ws) = (fp, inEffectInput v ws <> inEffectOutput v ws)
