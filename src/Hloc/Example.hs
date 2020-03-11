module Hloc.Example where

import Hloc.Hloc
import Hloc.Block
import Hloc.Blocks.Date
import Hloc.Blocks.Backlight
import Hloc.Blocks.CurrentWindow
import Hloc.Blocks.Volume

run :: IO ()
run = do
  date <- dateBlock [Minute, Plaintext ":", Sec]
  cw <- currentWindow 10000
  runHloc
    [ Block $ date
    , Block $ cw
    , Block $ backlightDefault
    , Block $ volumeDefault
    ]
