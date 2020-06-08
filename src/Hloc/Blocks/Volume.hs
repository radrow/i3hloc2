{-# LANGUAGE OverloadedStrings #-}
module Hloc.Blocks.Volume(volumeDefault, volume) where

import Data.Text as T
import Data.List as DL
import System.Clock
import qualified Sound.ALSA.Mixer as ALSA

import Hloc.Block

data Volume = Volume
  { meta         :: !BlockMeta
  , output       :: !Text
  , format       :: ![Format]
  , controlName  :: !String
  , mixerName    :: !String
  , delay        :: !Int
  , lastChange   :: !TimeSpec
  , boostTime    :: !TimeSpec
  , boost        :: !Bool
  }

data Format
  = Level
  | Mute
  | MuteAndLevel
  | Icon

volume :: BlockMeta -> Int -> [Format] -> String -> String -> Block
volume m d f mx con = Block $ Volume
  { meta = m
  , output = ""
  , format = f
  , controlName = con
  , mixerName = mx
  , delay = d
  , lastChange = TimeSpec {sec = 0, nsec = 0}
  , boostTime = TimeSpec {sec = 2, nsec = 0}
  , boost = False
  }

volumeDefault :: BlockMeta -> Block
volumeDefault m = volume m 1000000 [Icon, Level] "default" "Master"

wtfStr :: Text
wtfStr = "??"


twoDigitFront :: Text -> Text
twoDigitFront t = case T.length t of
  0 -> "00"
  1 -> "1" <> t
  _ -> t

getVolumePercent :: ALSA.Control -> IO (Maybe ALSA.CLong)
getVolumePercent control =
  case ALSA.playback (ALSA.volume control) of
    Nothing -> return Nothing
    Just volPlayback -> do
        (minV, maxV) <- ALSA.getRange volPlayback
        vm <- ALSA.getChannel ALSA.FrontLeft $ ALSA.value volPlayback
        return $ fmap (\v -> (v * 100) `quot` (maxV - minV)) vm

getVolumeMute :: ALSA.Control -> IO (Maybe Bool)
getVolumeMute control =
  case ALSA.playback (ALSA.switch control) of
    Nothing -> return Nothing
    Just switchPlayback ->
        fmap not <$> ALSA.getChannel ALSA.FrontLeft switchPlayback


instance IsBlock Volume where
  serialize b = [(serializationBase b){i3bFullText = output b}]
  update b = do
    newOutput <- ALSA.withMixer (mixerName b) $ \mixer -> do
      controlM <- ALSA.getControlByName mixer (controlName b)
      case controlM of
        Nothing -> return wtfStr
        Just control ->
          let showFormat = \case
                Icon -> do
                  mute <- getVolumeMute control
                  case mute of
                    Just True -> return iconMute
                    _ -> do
                      lvl <- getVolumePercent control
                      return $ case lvl of
                        Nothing -> wtfStr
                        Just n -> if | n > 50 -> icon2
                                     | n > 10 -> icon1
                                     | otherwise -> icon0
                Mute -> do
                  mute <- getVolumeMute control
                  return $ maybe wtfStr
                    (\m -> if m
                           then "off"
                           else "on"
                    ) mute
                Level -> do
                  lvl <- getVolumePercent control
                  return $ maybe wtfStr
                    (twoDigitFront . pack . show) lvl
                MuteAndLevel -> do
                  mute <- getVolumeMute control
                  lvl <- getVolumePercent control
                  case (mute, lvl) of
                    (Just m, Just l) ->
                      return $ if m
                               then "off"
                               else (twoDigitFront . pack . show) l
                    _ -> return wtfStr
          in T.concat . DL.intersperse " " <$> mapM showFormat (format b)
    timeNow <- getTime Realtime

    return b
      { output = newOutput
      , boost = timeNow - lastChange b < boostTime b
      , lastChange =
          if output b == newOutput
          then lastChange b
          else timeNow
      }
  waitTime b = if boost b then delay b `div` 10 else delay b
  getMeta = Just . meta


iconX :: Text
iconX = "\xf00d"

iconMute :: Text
iconMute = icon0 <> iconX

icon0 :: Text
icon0 = "\xf026"

icon1 :: Text
icon1 = "\xf027"

icon2 :: Text
icon2 = "\xf028"
