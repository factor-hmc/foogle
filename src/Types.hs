{-# LANGUAGE OverloadedStrings #-}
module Types where

import Data.Text

data FactorWord
  = FactorWord
      Text -- ^ The name of the word
      StackEffect -- ^ The stack effect of the word
  deriving (Read, Show, Eq)

data StackEffect 
  = StackEffect 
      [(Text, Maybe StackEffect)] -- ^ The input stack variables
      [(Text, Maybe StackEffect)] -- ^ The output stack variables
  deriving (Read, Show, Eq)

prettyWord :: FactorWord -> Text
prettyWord (FactorWord name effect) = name <> " " <> prettyEffect effect

prettyEffect :: StackEffect -> Text
prettyEffect (StackEffect ins outs) = "( " <> prettyEffect' ins <> " -- " <> prettyEffect' outs <> " )"

prettyEffect' :: [(Text, Maybe StackEffect)] -> Text
prettyEffect' [] = ""
prettyEffect' ((var, Just eff):vs) = var <> " " <> prettyEffect eff <> " " <> prettyEffect' vs
prettyEffect' ((var, Nothing):vs)  = var <> " " <> prettyEffect' vs
