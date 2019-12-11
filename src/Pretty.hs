{-# LANGUAGE OverloadedStrings #-}
module Pretty where

import Types
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (when)
import System.Console.ANSI
import System.Console.ANSI.Types

renderWordPlain :: SimpleWord -> Text
renderWordPlain (FactorWord name effect) = name <> " " <> renderEffectPlain effect

renderEffectPlain :: SimpleEffect -> Text
renderEffectPlain (StackEffect ins outs) = "( " <> renderEffectPlain' ins <> " -- " <> renderEffectPlain' outs <> " )"

renderEffectPlain' :: [(Text, Maybe SimpleEffect)] -> Text
renderEffectPlain' [] = ""
renderEffectPlain' ((var, Just eff):vs) = var <> " " <> renderEffectPlain eff <> " " <> renderEffectPlain' vs
renderEffectPlain' ((var, Nothing):vs)  = var <> " " <> renderEffectPlain' vs

renderWord :: HighlightedWord -> IO ()
renderWord (FactorWord name effect) = do
  renderHighlighted Red name
  TIO.putStr " "
  renderEffect effect

renderEffect :: HighlightedEffect -> IO ()
renderEffect (StackEffect ins outs) = do
  TIO.putStr "("
  renderEffect' Red ins
  TIO.putStr " --"
  renderEffect' Red outs
  TIO.putStr " )"

renderEffect' :: Color -> [(Highlighted, Maybe HighlightedEffect)] -> IO ()
renderEffect' _ [] = pure ()
renderEffect' color ((var, eff):vs) = do
  TIO.putStr " "
  renderHighlighted color var
  case eff of
    Just e -> do
      TIO.putStr " "
      renderEffect e
    Nothing -> pure ()
  renderEffect' color vs

renderHighlighted :: Color -> Highlighted -> IO ()
renderHighlighted color (Highlighted text highlights) = go 0 text (getHighlights highlights)
  where
    go i text _
      | T.null text = pure ()
    go i text [] = TIO.putStr text
    go i text highlights@((start, end):hs) 
      | i < start = do
        let Just (c, rest) = T.uncons text
        putChar c
        go (i+1) rest highlights
      | i > end = go i text hs
      -- end <= i <= start
      | otherwise = do
        let Just (c, rest) = T.uncons text
        when (i == start) $ setSGR [SetColor Foreground Vivid color]
        putChar c
        when (i == end) $ setSGR [Reset]
        go (i+1) rest highlights
