{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
module ParseEffect where

import Data.Bifunctor (first)
import Data.Char (isSpace)
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad (mzero)
import Control.Monad.Combinators
import Control.Applicative ((<|>))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import DB

parseEffect :: Text -> Either String Effect
parseEffect = first errorBundlePretty . parse stackEffect ""

type Parser = Parsec Void Text

symbol :: Text -> Parser Text
symbol s = L.symbol space1 s

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space1

reserveds :: [String]
reserveds = [")", "(", "--"]

nonSpace :: Parser Text
nonSpace = do
  str <- takeWhile1P (Just "nonSpace") (not . isSpace)
  if T.unpack str `elem` reserveds
    then mzero
    else pure str

effectVar :: Parser EffVar
effectVar = lexeme nonSpace >>= \effectName -> 
  if ":" `T.isSuffixOf` effectName
    then 
      let effectName' = T.init effectName
      in QuotEffVar effectName' <$> try (lexeme stackEffect)
         <|> TypedEffVar effectName' <$> lexeme nonSpace
    else 
      pure $ EffVar effectName

extractRowVar :: [EffVar] -> (Maybe EffRowVar, [EffVar])
extractRowVar [] = (Nothing, [])
extractRowVar vs@(EffVar v:vs')
  | ".." `T.isPrefixOf` v = (Just $ T.drop 2 v, vs')
  | otherwise           = (Nothing, vs)
extractRowVar vs = (Nothing, vs)

stackEffect :: Parser Effect
stackEffect = do
  symbol "("
  ins <- many (try effectVar)
  symbol "--"
  outs <- many (try effectVar)
  -- Doesn't have to have a space after it!
  chunk ")"
  let (effInVar, effIn) = extractRowVar ins
      (effOutVar, effOut) = extractRowVar outs
      -- TODO: check for this somehow (if at all possible).
      effTerminated = False
  pure $ Effect{..}
