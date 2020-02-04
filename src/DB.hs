{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module DB where

import Data.Aeson
import Data.Foldable (asum)
import Data.Text (Text(..))
import Control.Monad (guard)

-- | Datatype representing a variable in a stack effect.
-- Most variables are just 'EffVar's, but if a variable has
-- a colon and a following type or stack effect, it is made into
-- a specialized 'TypedEffVar' or 'QuotEffVar'.
data EffVar
  = EffVar Text
  -- ^ An effect variable
  | TypedEffVar Text Text
  -- ^ An effect variable with an associated type
  | QuotEffVar Text Effect
  -- ^ An effect variable with an associated stack effect
  deriving (Show)

type EffRowVar = Text

-- | Datatype representing a stack effect in Factor.
data Effect
  = Effect
    { effIn         :: [EffVar]
    -- ^ The input stack effects
    , effOut        :: [EffVar]
    -- ^ The output stack effects
    , effTerminated :: Bool
    -- ^ Whether the stack effect is guaranteed to throw an error
    -- or exception (in which case 'effOut' is ignored
    , effInVar      :: Maybe EffRowVar
    -- ^ The input row polymorphic variable (or 'Nothing', if there are none)
    , effOutVar     :: Maybe EffRowVar
    -- ^ The output row polymorphic variable (or 'Nothing', if there are none)
    }
    deriving (Show)

-- | Datatype representing a word in Factor.
data FactorWord
  = FactorWord
    { wordName :: Text
    -- ^ The name of the word
    , wordEff  :: Maybe Effect
    -- ^ The stack effect of the word
    }
    deriving (Show)


-- !! The FromJSON instances here are pretty brittle !!
-- They assume that the data is well-formed per how we're serializing from Factor.

instance FromJSON EffVar where
  parseJSON o = asum
      [ EffVar <$> parseJSON o
      , uncurry TypedEffVar <$> parseJSON o
      , uncurry QuotEffVar <$> parseJSON o
      ]

instance FromJSON Effect where
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

instance FromJSON FactorWord where
  parseJSON = withObject "factor word" $ \o -> do
    wordName <- o .: "name"
    wordEff  <- asum
      -- either we can parse it as an effect
      [ o .: "effect"
      -- or it's False
      , do b <- o .: "effect"
           guard (b == False)
           pure Nothing
      -- again, this is pretty brittle
      ]
    pure FactorWord{..}
