{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Parse where

import qualified System.FilePath.Find as F
-- import qualified System.IO.Strict as SIO
import Data.Bifunctor (first)
import Data.Char (isSpace)
import Control.Monad ((>=>), mzero)
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.ByteString as BS
import Control.Monad.Combinators
import Control.Applicative ((<|>))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Types

makeDB :: String -> IO [(FilePath, [FactorWord])]
makeDB dir = do
  paths <- F.find F.always (F.extension F.==? ".factor") dir
  traverse indexFile paths

indexFile :: FilePath -> IO (FilePath, [FactorWord])
indexFile fp = (fp,) . parseDefns fp . T.decodeUtf8 <$> BS.readFile fp

parseDefns :: String -> Text -> [FactorWord]
parseDefns fp = T.lines >=> parseDefn fp

parseDefn :: String -> Text -> [FactorWord]
parseDefn fp = either (const []) pure . parse factorWord fp

type Parser = Parsec Void Text

symbol :: Text -> Parser Text
symbol s = space *> L.symbol space1 s

reserveds :: [String]
reserveds = [":", "::", "(", "--", ")"]

nonSpace :: Parser Text
nonSpace = do
  str <- takeWhile1P (Just "nonSpace") (not . isSpace)
  if T.unpack str `elem` reserveds
    then mzero
    else pure str

stackEffect :: Parser StackEffect
stackEffect = do
  chunk "("
  space1
  ins <- (try stackEffectVar <|> try regularVar) `sepEndBy` space1
  chunk "--"
  space1
  outs <- (try stackEffectVar <|> try regularVar) `sepEndBy` space1
  chunk ")"
  pure $ StackEffect ins outs
  where
    regularVar = (\var -> (var, Nothing)) <$> nonSpace
    stackEffectVar = do
      var <- nonSpace
      space1
      se <- stackEffect
      pure (var, Just se)

factorWord :: Parser FactorWord
factorWord = do
  space
  try (chunk ":" *> space1) <|> (chunk "::" *> space1)
  name <- nonSpace
  space1
  se <- stackEffect
  pure $ FactorWord name se
