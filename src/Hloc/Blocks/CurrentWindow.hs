{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Hloc.Blocks.CurrentWindow(currentWindow) where

import Control.Exception
import Data.Text
import qualified Graphics.X11.Xlib as X11
import qualified Graphics.X11.Xlib.Extras as X11

import Hloc.Block


data CurrentWindow = CurrentWindow
  { meta    :: !BlockMeta
  , output  :: !(Maybe Text)
  , display :: !X11.Display
  , delay   :: !Int
  }


currentWindow :: BlockMeta -> Int -> IO Block
currentWindow m dl = do
  d <- X11.openDisplay ""
  o <- getCurrentWindowName d
  return $ Block $ CurrentWindow
    { meta = m
    , output = o
    , display = d
    , delay = dl
    }


getCurrentWindowName :: X11.Display -> IO (Maybe Text)
getCurrentWindowName d = do
  (w, _) <- X11.getInputFocus d
  a <- X11.internAtom d "_NET_WM_NAME" False
  wname <- handle (\(_ :: SomeException) -> return "") $ do
    p <- X11.getTextProperty d w a
    Prelude.concat <$> X11.wcTextPropertyToTextList d p
  return $ case wname of
    [] -> Nothing
    _  -> Just (pack wname)


instance IsBlock CurrentWindow where
  waitTime = delay
  serialize b = case output b of
    Just txt -> [(serializationBase b){ i3bFullText = txt }]
    Nothing -> []
  update b = do
    o <- getCurrentWindowName (display b)
    return b{output = o}
  getMeta = Just . meta
