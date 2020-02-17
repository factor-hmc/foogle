{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Parse (parseEffect)
import Types
import Search (searchDB)
import Infer (infer)
-- import Argparse

import Data.Aeson
import Data.Bifunctor (bimap)
import Data.Bool (bool)
import Data.Functor ((<&>))
-- import Data.Foldable (traverse_)
import Data.Text (Text(..))
-- import qualified Data.Text as T
-- import qualified Data.Text.IO as T
import qualified Data.ByteString.Lazy as BS
-- import Options.Applicative (execParser)
import Control.Arrow ((>>>))

import           Miso
import           Miso.String (MisoString)
import qualified Miso.String as S
-- For reading files
import           Control.Concurrent.MVar

-- For reading files
import GHCJS.Types
import GHCJS.Foreign.Callback

data Model
  = Model
  { _db         :: Either MisoString [FactorWord Text]
  , _query      :: Text
  , _numResults :: Int
  , _results    :: [FactorWord Text]
  }
  deriving (Eq)

initialModel :: Model
initialModel = Model
  { _db         = Left $ "Database not loaded."
  , _query      = mempty
  , _numResults = 5
  , _results    = []
  }

data Action
  = NoOp
  | LoadDB
  | UpdateDB (Either MisoString [FactorWord Text])
  | UpdateQuery MisoString
  | UpdateNumResults Int
  | MakeQuery

main :: IO ()
main = startApp $ App{..}
  where
    initialAction = NoOp    -- initial action to be executed on application load
    model  = initialModel   -- initial model
    update = updateModel    -- update function
    view   = viewModel      -- view function
    events = defaultEvents  -- default delegated events
    subs   = []             -- empty subscription list
    mountPoint = Nothing    -- mount point for application (Nothing defaults to 'body')

updateModel :: Action -> Model -> Miso.Effect Action Model
updateModel NoOp m = noEff m
updateModel LoadDB m = m <# do
  fileReaderInput <- getElementById "fileReader"
  file <- getFile fileReaderInput
  reader <- newReader
  mvar <- newEmptyMVar
  setOnLoad reader =<< do
    asyncCallback $ do
      r <- getResult reader
      putMVar mvar r
  readText reader file
  readMVar mvar <&>
    (   S.fromMisoString                 -- Convert to bytestring
    >>> eitherDecode                     -- Decode to JSON
    >>> bimap S.toMisoString (map infer) -- Convert errors to JS strings; infer successful decodes
    >>> UpdateDB                         -- Update the DB
    )
updateModel (UpdateDB db) m        = noEff m { _db = db }
updateModel (UpdateQuery q) m      = noEff m { _query = S.fromMisoString q }
updateModel (UpdateNumResults n) m = noEff m { _numResults = n }
updateModel MakeQuery m@Model{..}  = noEff m { _results = results }
  where
    results = case (_db, parseEffect _query) of
      (Right db, Right qEff) -> searchDB _numResults qEff db
      -- Just silently fail for now
      _ -> []

viewModel :: Model -> View Action
viewModel Model{..} = div_ []
  [ h1_ [] [ text "Foogle Web 0.1" ]
  , input_
      [ placeholder_ "Search Foogle"
      , autofocus_ True
      , value_ $ S.toMisoString _query
      , name_ "query"
      , onInput UpdateQuery
      , onEnter MakeQuery
      ]
  , div_ [] $ [ text $ either id (const "") _db ]
  , div_ [] $ 
      map viewResult _results
  , input_ [ id_ "fileReader"
           , type_ "file"
           , onChange (const LoadDB)
           ]
  ]

onEnter :: Action -> Attribute Action
onEnter action =
  onKeyDown $ bool NoOp action . (== KeyCode 13) 

viewResult :: FactorWord Text -> View Action
viewResult fw = div_ [] [text . S.toMisoString . show $ fw]
  
foreign import javascript unsafe "$r = new FileReader();"
  newReader :: IO JSVal

foreign import javascript unsafe "$r = $1.files[0];"
  getFile :: JSVal -> IO JSVal

foreign import javascript unsafe "$1.onload = $2;"
  setOnLoad :: JSVal -> Callback (IO ()) -> IO ()

foreign import javascript unsafe "$r = $1.result;"
  getResult :: JSVal -> IO MisoString

foreign import javascript unsafe "$1.readAsText($2);"
  readText :: JSVal -> JSVal -> IO ()

-- Previous main (for commandline foogle)
-- main :: IO ()
-- main = execParser optParser >>= run
-- 
-- run :: Query -> IO ()
-- run Query{..} = withDB $ \db ->
--   let inferredDB = map infer db
--   in traverse_ print $ searchDB queryNumResults queryStackEffect inferredDB
-- 
-- withDB :: ([FactorWord Text] -> IO ()) -> IO ()
-- withDB f = do
--   (eitherDecode <$> BS.readFile "db.json") >>= \case
--     Left err  -> putStrLn err
--     Right db -> f db 
