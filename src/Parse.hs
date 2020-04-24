{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
module Parse where

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

import Types

parseEffect :: Text -> Either String (Effect Text)
parseEffect = first errorBundlePretty . parse stackEffect ""

type Parser = Parsec Void Text

symbol :: Text -> Parser Text
symbol s = L.symbol space1 s

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space1

reserveds :: [String]
reserveds = [ ")"
            , "("
            , "--"
            , "–"
            , "—"
            -- This is not used in Factor, but is included for convenience
            , "|"]

nonSpace :: Parser Text
nonSpace = do
  str <- takeWhile1P (Just "nonSpace") (not . isSpace)
  if T.unpack str `elem` reserveds
    then mzero
    else pure str

quotVar :: Text -> Parser (EffVar Text)
quotVar name = QuotEffVar name <$> lexeme stackEffect

typedVar :: Text -> Parser (EffVar Text)
typedVar name = do
  -- Factor will only parse a single type in a type variable,
  -- but we will parse a union type separated by "|", e.g.
  --
  --  ( numOrStr: number | string bool: boolean -- output )
  --
  --  It can be a little hard to read, so you can put a newline
  --
  --  ( numOrStr: number | string
  --    bool:     boolean
  --    --
  --    output
  --  )
  tps <- lexeme nonSpace `sepBy1` symbol "|"
  pure $ TypedEffVar name tps

effectVar :: Parser (EffVar Text)
effectVar = lexeme nonSpace >>= \effectName ->
  if ":" `T.isSuffixOf` effectName
    then
      let effectName' = T.init effectName
      in try (quotVar effectName') <|> typedVar effectName'
    else
      pure $ EffVar effectName

extractRowVar :: [EffVar Text] -> (Maybe Text, [EffVar Text])
extractRowVar [] = (Nothing, [])
extractRowVar vs@(EffVar v:vs')
  | ".." `T.isPrefixOf` v = (Just $ T.drop 2 v, vs')
  | otherwise           = (Nothing, vs)
extractRowVar vs = (Nothing, vs)

stackEffect :: Parser (Effect Text)
stackEffect = do
  symbol "("
  ins <- many (try effectVar)
  -- Allow "—" or "–" to be used instead of "--" 
  -- (hotfix for users who can't type "--" because their phones autocorrect it 
  -- to an endash or an emdash)
  try (symbol "--") <|> try (symbol "—") <|> symbol "–"
  outs <- many (try effectVar)
  -- Doesn't have to have a space after it!
  chunk ")"
  let (effInVar, effIn) = extractRowVar ins
      (effOutVar, effOut) = extractRowVar outs
      -- TODO: check for this somehow (if at all possible).
      effTerminated = False
  pure $ Effect{..}
