module Argparse where

import Types
import Parse
import Text.Read (readMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative


stackEffectParser :: ReadM (Effect Text)
stackEffectParser = eitherReader toStackEffect
  where
    toStackEffect :: String -> Either String (Effect Text)
    toStackEffect = parseEffect . T.pack

data Query
  = Query
  { queryNumResults  :: Int
  , queryStackEffect :: Effect Text
  }

options :: Options.Applicative.Parser Query
options = Query
        <$> option auto
            ( long "numResults"
           <> short 'n'
           <> help "Number of results displayed"
           <> value 5 
           <> metavar "INT")
        <*> argument stackEffectParser
            ( metavar "QUERY"
           <> help "Input stack effect to query database")

optParser :: ParserInfo Query
optParser = info (options <**> helper)
            ( fullDesc
           <> progDesc "Search by stack effect for words in Factor"
           <> header "Foogle - Hoogle for Factor" )
