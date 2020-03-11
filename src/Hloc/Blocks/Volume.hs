module Hloc.Blocks.Volume(volumeDefault, volume) where

import System.Clock
import qualified Sound.ALSA.Mixer as ALSA

import Hloc.Block

data Volume = Volume
  { output :: String
  , controlName :: String
  , mixerName :: String
  , delay :: Int
  , lastChange   :: TimeSpec
  , boostTime    :: TimeSpec
  , boost        :: Bool
  }

volume :: Int -> String -> String -> Volume
volume d mx con = Volume
  { output = ""
  , controlName = con
  , mixerName = mx
  , delay = d
  , lastChange = TimeSpec {sec = 0, nsec = 0}
  , boostTime = TimeSpec {sec = 1, nsec = 0}
  , boost = False
  }

volumeDefault :: Volume
volumeDefault = volume 200000 "default" "Master"

volumeWtf :: Volume -> Volume
volumeWtf v = v {output = "??"}


twoDigitFront :: String -> String
twoDigitFront s | null s        = "00"
                | length s == 1 = '0':s
                | otherwise     = s


instance IsBlock Volume where
  serialize b = pure defaultBlock {fullText = output b}
  update b = ALSA.withMixer (mixerName b) $ \mixer -> do
    controlM <- ALSA.getControlByName mixer (controlName b)
    case controlM >>= ALSA.playback . ALSA.volume of
      Nothing -> return $ volumeWtf b
      Just volPlayback -> do
        (minV, maxV) <- ALSA.getRange volPlayback
        vm <- ALSA.getChannel ALSA.FrontLeft $ ALSA.value volPlayback
        timeNow <- getTime Realtime
        case vm of
          Nothing -> return $ volumeWtf b
          Just v ->
            let newOutput = twoDigitFront $ show ((v * 100) `quot` (maxV - minV))
            in return b
               { output = twoDigitFront $ show ((v * 100) `quot` (maxV - minV))
               , boost = timeNow - lastChange b < boostTime b
               , lastChange =
                 if output b == newOutput
                 then lastChange b
                 else timeNow
               }
  waitTime b = if boost b then delay b `div` 10 else delay b
