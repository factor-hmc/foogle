{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleInstances #-}

module Types where

import Data.Aeson
import Data.Foldable (asum)
import Data.Text (Text(..))
import qualified Data.Text as T
import Control.Monad (guard)

-- | Datatype representing a variable in a stack effect.
-- Most variables are just 'EffVar's, but if a variable has
-- a colon and a following type or stack effect, it is made into
-- a specialized 'TypedEffVar' or 'QuotEffVar'.
data EffVar a
  = EffVar a
  -- ^ An effect variable
  | TypedEffVar a a
  -- ^ An effect variable with an associated type
  | QuotEffVar a (Effect a)
  -- ^ An effect variable with an associated stack effect
  deriving (Eq)

effVarName :: EffVar a -> a
effVarName (EffVar a) = a
effVarName (TypedEffVar a _) = a
effVarName (QuotEffVar a _) = a

-- | Datatype representing a stack effect in Factor.
data Effect a
  = Effect
    { effIn         :: [EffVar a]
    -- ^ The input stack effects
    , effOut        :: [EffVar a]
    -- ^ The output stack effects
    , effTerminated :: Bool
    -- ^ Whether the stack effect is guaranteed to throw an error
    -- or exception (in which case 'effOut' is ignored) - we expect
    -- this to be rare and won't be using it for now.
    , effInVar      :: Maybe a
    -- ^ The input row polymorphic variable (or 'Nothing', if there are none)
    , effOutVar     :: Maybe a
    -- ^ The output row polymorphic variable (or 'Nothing', if there are none)
    }
    deriving (Eq)

-- | Datatype representing a word in Factor.
data FactorWord a
  = FactorWord
    { wordName :: a
    -- ^ The name of the word
    , wordEff  :: Maybe (Effect a)
    -- ^ The stack effect of the word
    , wordURL :: a
    -- ^ The documentation location of the word
    }

-- Can't go from 'a' to 'b' because there are row variables that 
-- don't get mapped over.
overEffVars :: (EffVar a -> EffVar a) -> Effect a -> Effect a
overEffVars f Effect{..} = Effect
  { effIn         = map f effIn
  , effOut        = map f effOut
  , effTerminated = effTerminated
  , effInVar      = effInVar
  , effOutVar     = effOutVar
  }

instance Show (EffVar Text) where
  show (EffVar v) = T.unpack v
  show (TypedEffVar v t) = T.unpack v <> ": " <> T.unpack t
  show (QuotEffVar v eff) = T.unpack v <> ": " <> show eff

instance Show (Effect Text) where
  show Effect{..} = concat
      [ "( "
      , convertRowVar effInVar
      , convertVars effIn
      , "-- "
      , convertRowVar effOutVar
      , convertVars effOut
      , ")"
      ]
    where
      convertRowVar = maybe "" ((".." <>) . (<> " ")) . fmap T.unpack
      convertVars = concatMap ((<> " ") . show)

instance Show (FactorWord Text) where
  show FactorWord{..} = mconcat 
    [ ": "
    , T.unpack wordName
    , maybe "" ((" " <>) . show) wordEff
    , " (" <> T.unpack wordURL <> ")"
    ]

-- !! The FromJSON instances here are pretty brittle !!
-- They assume that the data is well-formed per how we're serializing from Factor.

instance FromJSON (EffVar Text) where
  parseJSON o = asum
      [ EffVar <$> parseJSON o
      , uncurry TypedEffVar <$> parseJSON o
      , uncurry QuotEffVar <$> parseJSON o
      ]

instance FromJSON (Effect Text) where
  parseJSON = withObject "stack effect" $ \o -> do
    effIn         <- o .: "in"
    effOut        <- o .: "out"
    effTerminated <- o .: "terminated?"
    effInVar      <- parseRowVar o "in_var"
    effOutVar     <- parseRowVar o "out_var"
    pure Effect{..}
    where
      parseEffVar field = asum
      parseRowVar o field = asum
        -- either we can parse it as a row var
        [ o .: field
        -- or it's False
        , do b <- o .: field
             guard (b == False)
             pure Nothing
        -- it shouldn't be anything else, and we therefore won't be lenient here.
        -- if the parser breaks, it could be because of this.
        ]

instance FromJSON (FactorWord Text) where
  parseJSON = withObject "factor word" $ \o -> do
    wordName <- o .: "name"
    wordEff  <- asum
      -- either we can parse it as an effect
      [ o .: "effect"
      -- or it's False
      , do b <- o .: "effect"
           guard (b == False)
           pure Nothing
      -- it shouldn't be anything else (also pretty brittle)
      ]
    wordURL <- o .: "url"
    pure FactorWord{..}
