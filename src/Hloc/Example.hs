module Hloc.Example where

import Hloc.Hloc
import Hloc.I3Bar
import Hloc.Block
import Hloc.Blocks.Date
import Hloc.Blocks.Backlight
import Hloc.Blocks.CurrentWindow
import Hloc.Blocks.Volume
import Hloc.Blocks.Network
import Hloc.Blocks.Battery as Bat

run :: IO ()
run = do
  date <- dateBlock [Minute, Plaintext ":", Sec]
  cw <- currentWindow 10000
  net <- networkDefault "wlo1"
  bat <- batteryDefault "BAT0" [Bat.Status, Health, Charge, ETA]
  runHloc defaultHeader
    [ Block date
    -- , Block cw
    -- , Block backlightDefault
    -- , Block volumeDefault
    -- , Block net
    -- , Block bat
    ]
