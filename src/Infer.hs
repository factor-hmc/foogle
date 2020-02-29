{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Infer where

import Types
import Data.Text (Text(..))
import qualified Data.Text as T
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)

-- | Remove any extraneous features of a word name 
-- (used for inferring its type).
stripExtraneous :: Text -> Text
stripExtraneous t = 
  -- Remove any trailing quotes or digits
  T.dropWhileEnd (\x -> x == '\'' || isDigit x) t''
  where
    -- Drop "new" from the front
    t'  = fromMaybe t  $ T.stripPrefix "new" t
    -- Drop "/f" from the end
    t'' = fromMaybe t' $ T.stripSuffix "/f" t'

-- | Infer a type from a variable name
-- (types are just 'Text'). Taken from Factor's
-- tools.scaffold.private function 'lookup-type'.
-- This is pretty naive.
inferType :: Text -> Maybe Text
inferType name = lookup (T.toLower $ stripExtraneous name)
  [ ("seq", "sequence")
  , ("sequence", "sequence")
  , ("array", "array")
  , ("arr", "array")
  , ("string", "string")
  , ("str", "string")
  , ("quot", "quotation")
  , ("quotation", "quotation")
  , ("assoc", "assoc")
  , ("class", "class")
  , ("timer", "timer")
  , ("url", "url")
  , ("byte-array", "byte-array")
  , ("byte-arr", "byte-array")
  , ("word", "word")
  , ("duration", "duration")
  , ("hash", "hashtable")
  , ("hashtable", "hashtable")
  , ("c-ptr", "c-ptr")
  , ("keys", "sequence")
  , ("?", "boolean")
  , ("boolean", "boolean")
  , ("n", "number")
  , ("num", "number")
  , ("number", "number")
  ]

-- | Infer a type for an 'EffVar' if it doesn't have one already.
inferEffVar :: EffVar Text -> EffVar Text
inferEffVar (EffVar v) = case inferType v of
  -- We can't infer any type, so don't change it
  Nothing -> EffVar v
  -- We can infer a type, so give it the type
  Just tp -> TypedEffVar v tp
inferEffVar (TypedEffVar v tp) = TypedEffVar v tp
inferEffVar (QuotEffVar v eff) = QuotEffVar v (overEffVars inferEffVar eff)

infer :: FactorWord Text -> FactorWord Text
infer FactorWord{..} = FactorWord
  { wordName = wordName
  , wordEff = overEffVars inferEffVar <$> wordEff
  , wordURL = wordURL
  }
