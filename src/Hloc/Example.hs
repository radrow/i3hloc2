{-# LANGUAGE OverloadedStrings #-}
module Hloc.Example where

import Hloc.Hloc
import Hloc.I3Bar
import Hloc.Block
import Hloc.Color
import Hloc.Blocks.Date
import Hloc.Blocks.Backlight
import Hloc.Blocks.CurrentWindow
import Hloc.Blocks.Volume
import Hloc.Blocks.SystemCommand
import Hloc.Blocks.Network
import Hloc.Blocks.Battery as Bat

run :: IO ()
run = do
  let m = defaultMeta
  date <- dateBlock m{bmColor = Just paleTurquoise}
          ["\xf073 ", Day, "-", Month, "-", Year]
  time <- dateBlock m{bmColor = Just pink}
          ["\xf017 ", Hour24, ":", Minute, ":", Sec]
  cw <- currentWindow m{bmBorderBottom = 1} 1000000
  let netConf = networkDefaultConfig m{ bmColor = Just lime
                                      , bmMinWidth = Just 230
                                      , bmAlign = AlignCenter
                                      } "wlo1"
  net <- network netConf{ncFormat = "\xf1eb": ncFormat netConf}
  bat <- batteryDefault m "BAT0"
    [Bat.Status True, Health, Charge True, Charge False, ETA]
  runHloc defaultHeader
    [ time
    , date
    , backlightDefault m{bmColor = Just white}
--    , volumeDefault m{bmColor = Just yellow}
    , net
    , systemCommandSimple m{bmColor = Just red} "hostname" ["-i"] 100000000
    , bat
    , cw
    ]

