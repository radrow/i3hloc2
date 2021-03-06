{-# LANGUAGE OverloadedStrings #-}
module Hloc.Blocks.Backlight
  ( backlight, backlightDefault
  ) where

import           Data.Text
import           Control.Applicative
import           System.Clock
import           System.FilePath
import           System.IO             hiding (hGetContents)
import           System.IO.Strict
import           Text.Read

import           Hloc.Block


data Backlight = Backlight
  { meta         :: !BlockMeta
  , delay        :: !Int
  , lastChange   :: !TimeSpec
  , boostTime    :: !TimeSpec
  , boost        :: !Bool
  , backlightDir :: !FilePath
  , showIcon     :: !Bool
  , output       :: !(Maybe Int)
  }

backlight :: BlockMeta -> Int -> FilePath -> Bool -> Block
backlight m d bd ic = Block $ Backlight
  { meta = m
  , delay = d
  , lastChange = TimeSpec {sec = 0, nsec = 0}
  , boostTime = TimeSpec {sec = 2, nsec = 0}
  , boost = False
  , backlightDir = bd
  , showIcon = ic
  , output = Nothing
  }

backlightDefault :: BlockMeta -> Block
backlightDefault m = backlight m 1000000 "/sys/class/backlight/intel_backlight" True

instance IsBlock Backlight where
  waitTime b = if boost b then delay b `div` 10 else delay b
  serialize b = case output b of
    Just val ->
      [(serializationBase b)
        { i3bFullText =
            pack ((if showIcon b then "\xf185 " else "") <> show val)
        }]
    Nothing -> []
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
