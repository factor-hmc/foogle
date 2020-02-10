module Old.Argparse where

import Old.Types
import Text.Read (readMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative


-- There are things going on here that really probably shouldn't be...
maybeParser :: Read a => ReadM (Maybe a)
maybeParser = maybeReader toInt
  where
    -- Convert to a Maybe Int on successful parse
    toInt :: Read a => String -> Maybe (Maybe a)
    toInt a = Just <$> readMaybe a

maybeText :: ReadM (Maybe Text)
maybeText = maybeReader (Just . Just . T.pack)

queries :: ReadM [Text]
queries = maybeReader queries'
  where
    -- only parse one input and unconditionally succeed for now,
    -- we need to do some thinking about how to handle this...
    queries' :: String -> Maybe [Text]
    queries' = Just . pure . T.pack

options :: Parser Query
options = Query
        <$> option maybeParser
            ( long "inMin"
           <> help "Minimum number of inputs to the word"
           <> value Nothing
           <> metavar "INT")
        <*> option maybeParser
            ( long "inMax"
           <> help "Maximum number of inputs to the word"
           <> value Nothing
           <> metavar "INT")
        <*> option queries
            ( long "inQueries"
           <> short 'i'
           <> value []
           <> help "Queries for the inputs to the word")
        <*> option maybeParser
            ( long "outMin"
           <> help "Minimum number of outputs to the word"
           <> value Nothing
           <> metavar "INT")
        <*> option maybeParser
            ( long "outMax"
           <> help "Maximum number of outputs to the word"
           <> value Nothing
           <> metavar "INT")
        <*> option queries
            ( long "outQueries"
           <> short 'o'
           <> value []
           <> help "Queries for the outputs to the word")
        <*> option maybeText
            ( long "word"
           <> short 'w'
           <> value Nothing
           <> help "Name of the word")
        <*> switch
            ( long "strict"
           <> short 's'
           <> help "Strictly match inputs")
        <*> strArgument
            ( metavar "PATH"
           <> help "Filename or directory to index")

optParser :: ParserInfo Query
optParser = info (options <**> helper)
            ( fullDesc
           <> progDesc "Search for words in Factor"
           <> header "Foogle - Hoogle for Factor" )
