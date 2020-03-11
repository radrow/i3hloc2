{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
module Hloc.I3Bar where


import Data.Aeson
import System.Posix.Signals

import Hloc.Color


data I3BarHeader = I3BarHeader
  { version :: Int
  , stopSignal :: Signal
  , contSignal :: Signal
  , clickEvents :: Bool
  }

-- |Default values of the header according to the protocol
defaultHeader :: I3BarHeader
defaultHeader = I3BarHeader
  { version = 1
  , stopSignal = sigSTOP
  , contSignal = sigCONT
  , clickEvents = True
  }
instance ToJSON I3BarHeader where
  toJSON h = object
    [ "version" .= version h
    , "stop_signal" .= toInteger (stopSignal h)
    , "cont_signal" .= toInteger (contSignal h)
    , "click_events" .= clickEvents h
    ]

data I3BarBlock = I3BarBlock
  { fullText :: String
  , shortText :: Maybe String
  , color :: Maybe Color
  , background :: Maybe Color
  , border :: Maybe Color
  , borderBottom :: Int
  , borderTop :: Int
  , borderRight :: Int
  , borderLeft :: Int
  , minWidth :: Maybe Int
  , align :: Align
  , name :: Maybe String
  , instance_ :: Maybe String
  , urgent :: Bool
  , separator :: Bool
  , separatorBlockWidth :: Int
  , markup :: Markup
  }


-- | Default values of the block according to the protocol
defaultBlock :: I3BarBlock
defaultBlock = I3BarBlock
  { fullText = ""
  , shortText = Nothing
  , color = Nothing
  , background = Nothing
  , border = Nothing
  , borderBottom = 1
  , borderTop = 1
  , borderRight = 1
  , borderLeft = 1
  , minWidth = Nothing
  , align = AlignLeft
  , name = Nothing
  , instance_ = Nothing
  , urgent = False
  , separator = True
  , separatorBlockWidth = 9
  , markup = NoMarkup
  }
instance ToJSON I3BarBlock where
  toJSON b = object -- FIXME are nulls equivalent to no value?
    [ "full_text" .= fullText b
    , "short_text" .= shortText b
    , "color" .= color b
    , "background" .= background b
    , "border" .= border b
    , "border_bottom" .= borderBottom b
    , "border_top" .= borderTop b
    , "border_right" .= borderRight b
    , "border_left" .= borderLeft b
    , "min_width" .= minWidth b
    , "align" .= align b
    , "name" .= name b
    , "instance" .= instance_ b
    , "urgent" .= urgent b
    , "separator" .= separator b
    , "separator_block_width" .= separatorBlockWidth b
    , "markup" .= markup b
    ]

data Align = AlignCenter | AlignLeft | AlignRight
instance ToJSON Align where
  toJSON AlignCenter = "center"
  toJSON AlignLeft = "left"
  toJSON AlignRight = "right"

data Markup = Pango | NoMarkup
instance ToJSON Markup where
  toJSON Pango = "pango"
  toJSON NoMarkup = "none"


data ClickEvent = ClickEvent
  { blockName :: String
  , blockInstance :: String
  , fixedX :: Int
  , fixedY :: Int
  , relativeX :: Int
  , relativeY :: Int
  , blockWidth :: Int
  , blockHeight :: Int
  , modifiers :: [String]
  }

instance FromJSON ClickEvent where
  parseJSON (Object v) = ClickEvent
    <$> v .: "name"
    <*> v .: "instance"
    <*> v .: "x"
    <*> v .: "y"
    <*> v .: "relative_x"
    <*> v .: "relative_y"
    <*> v .: "width"
    <*> v .: "height"
    <*> v .: "modifiers"
