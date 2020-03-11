module Hloc.Blocks.CurrentWindow(currentWindow) where

import Data.Maybe(fromMaybe)
import qualified Graphics.X11.Xlib as X11
import qualified Graphics.X11.Xlib.Extras as X11

import Hloc.Block

data CurrentWindow = CurrentWindow
  { output :: String
  , display :: X11.Display
  , delay :: Int
  }

currentWindow :: Int -> IO CurrentWindow
currentWindow dl = do
  d <- X11.openDisplay ""
  o <- getCurrentWindowName d
  return CurrentWindow
    { output = o
    , display = d
    , delay = dl
    }


getCurrentWindowName :: X11.Display -> IO String
getCurrentWindowName d = do
  (w, _) <- X11.getInputFocus d
  fromMaybe "???" <$> X11.fetchName d w


instance IsBlock CurrentWindow where
  waitTime = delay
  serialize b = pure defaultBlock { fullText = output b }
  update b = do
    o <- getCurrentWindowName (display b)
    return b{output = o}
