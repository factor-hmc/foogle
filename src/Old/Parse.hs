{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Old.Parse where

import qualified System.FilePath.Find as F
-- import qualified System.IO.Strict as SIO
import Data.Bifunctor (first)
import Data.Char (isSpace)
import Control.Monad ((>=>), mzero)
import Data.Void
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Error as T
import qualified Data.Text.IO as T
import qualified Data.ByteString as BS
import Control.Monad.Combinators
import Control.Applicative ((<|>))
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Old.Types

makeDB :: String -> IO [(FilePath, [SimpleWord])]
makeDB dir = do
  paths <- F.find F.always (F.extension F.==? ".factor") dir
  traverse indexFile paths

indexFile :: FilePath -> IO (FilePath, [SimpleWord])
indexFile fp = (fp,) . parseDefns fp . T.decodeUtf8With T.lenientDecode <$> BS.readFile fp

parseDefns :: String -> Text -> [SimpleWord]
parseDefns fp = T.lines >=> parseDefn fp

parseDefn :: String -> Text -> [SimpleWord]
parseDefn fp = either (const []) pure . parse factorWord fp

type Parser = Parsec Void Text

symbol :: Text -> Parser Text
symbol s = L.symbol space1 s

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space1

reserveds :: [String]
reserveds = [":", "::", "(", "--", ")"]

nonSpace :: Parser Text
nonSpace = do
  str <- takeWhile1P (Just "nonSpace") (not . isSpace)
  if T.unpack str `elem` reserveds
    then mzero
    else pure str

stackEffect :: Parser SimpleEffect
stackEffect = do
  symbol "("
  ins <- many (try stackEffectVar <|> try regularVar)
  symbol "--"
  outs <- many (try stackEffectVar <|> try regularVar)
  -- Doesn't have to have a space after it!
  chunk ")"
  pure $ StackEffect ins outs
  where
    regularVar = (, Nothing) <$> lexeme nonSpace
    stackEffectVar = do
      var <- lexeme nonSpace
      se <- lexeme stackEffect
      pure (var, Just se)

factorWord :: Parser SimpleWord
factorWord = do
  space
  try (symbol ":") <|> symbol "::"
  name <- lexeme nonSpace
  se <- stackEffect
  space
  pure $ FactorWord name se