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
  [ ("obj", "object")
  , ("object", "object")
  , ("exemplar", "object")
  , ("seq", "sequence")
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
  ]

-- | Infer a type for an 'EffVar' if it doesn't have one already.
inferEffVar :: EffVar Text -> EffVar Text
inferEffVar (EffVar v) = case inferType v of
  -- No successful inference
  Nothing -> EffVar v
  -- Inferred a type
  Just tp -> TypedEffVar v tp
-- There is already a type
inferEffVar (TypedEffVar v tp) = TypedEffVar v tp
inferEffVar (QuotEffVar v eff) = QuotEffVar v (overEffVars inferEffVar eff)
