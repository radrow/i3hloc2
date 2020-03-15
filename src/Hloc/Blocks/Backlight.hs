module Hloc.Blocks.Backlight
  ( backlight, backlightDefault
  ) where

import           Control.Applicative
import           System.Clock
import           System.FilePath
import           System.IO             hiding (hGetContents)
import           System.IO.Strict
import           Text.Read

import           Hloc.Block


data Backlight = Backlight
  { delay        :: Int
  , lastChange   :: TimeSpec
  , boostTime    :: TimeSpec
  , boost        :: Bool
  , backlightDir :: FilePath
  , output       :: Maybe Int
  }

backlight :: Int -> FilePath -> Backlight
backlight d bd = Backlight
  { delay = d
  , lastChange = TimeSpec {sec = 0, nsec = 0}
  , boostTime = TimeSpec {sec = 1, nsec = 0}
  , boost = False
  , backlightDir = bd
  , output = Nothing
  }

backlightDefault :: Backlight
backlightDefault = backlight 200000 "/sys/class/backlight/intel_backlight"

instance IsBlock Backlight where
  waitTime b = if boost b then delay b `div` 10 else delay b
  serialize b = pure defaultBlock
    { fullText = maybe "" show (output b)
    }
  update b = do
    let brightnessFile = backlightDir b </> "brightness"
    let maxBrightnessFile = backlightDir b </> "max_brightness"
    brightnessStr <- withFile brightnessFile ReadMode hGetContents
    maxBrightnessStr <- withFile maxBrightnessFile ReadMode hGetContents

    timeNow <- getTime Realtime

    case liftA2 (,) (readMaybe brightnessStr) (readMaybe maxBrightnessStr) of
      Nothing -> error "parse error"
      Just (br, mbr) -> do
        let newOutput = (br * 100) `div` mbr
        return b{ output = Just newOutput
                , boost = timeNow - lastChange b < boostTime b
                , lastChange =
                    if output b == Just newOutput
                    then lastChange b
                    else timeNow
                }
