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
import Hloc.Blocks.Network
import Hloc.Blocks.Battery as Bat

run :: IO ()
run = do
  let m = defaultMeta
  date <- dateBlock m{bmColor = Just $ light green} [Day, "-", Month, "-", Year]
  time <- dateBlock m{bmColor = Just $ light red} [Hour24, ":", Minute, ":", Sec]
  cw <- currentWindow m 1000000
  net <- networkDefault m{bmColor = Just green} "wlo1"
  bat <- batteryDefault m "BAT0" [Bat.Status, Health, Charge, ETA]
  runHloc defaultHeader
    [ time
    , date
    , cw
    , backlightDefault m{bmColor = Just (RGB 230 230 20)}
    , volumeDefault m{bmColor = Just (RGB 230 20 230)}
    , net
    , bat
    ]

