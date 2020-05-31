{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
module Hloc.I3Bar where

import Control.Lens
import Control.Lens.Internal.FieldTH(_fieldToDef)
import Language.Haskell.TH.Syntax(mkName, nameBase)
import Hloc.Aeson
import Data.Aeson
import Data.Aeson.TH
import Data.Text(Text)
import System.Posix.Signals

import Hloc.Color

data I3BarHeader = I3BarHeader
  { i3hVersion :: Int
  , i3hStopSignal :: Int
  , i3hContSignal :: Int
  , i3hClickEvents :: Bool
  }

deriveToJSON deriveOptions ''I3BarHeader

-- |Default values of the header according to the protocol
defaultHeader :: I3BarHeader
defaultHeader = I3BarHeader
  { i3hVersion = 1
  , i3hStopSignal = fromIntegral sigSTOP
  , i3hContSignal = fromIntegral sigCONT
  , i3hClickEvents = True
  }


data Align = AlignCenter | AlignLeft | AlignRight
instance ToJSON Align where
  toJSON AlignCenter = "center"
  toJSON AlignLeft = "left"
  toJSON AlignRight = "right"

data Markup = Pango | NoMarkup
instance ToJSON Markup where
  toJSON Pango = "pango"
  toJSON NoMarkup = "none"

data I3BarBlock = I3BarBlock
  { i3bFullText :: Text
  , i3bShortText :: Maybe Text
  , i3bColor :: Maybe Color
  , i3bBackground :: Maybe Color
  , i3bBorder :: Maybe Color
  , i3bBorderBottom :: Int
  , i3bBorderTop :: Int
  , i3bBorderRight :: Int
  , i3bBorderLeft :: Int
  , i3bMinWidth :: Maybe Int
  , i3bAlign :: Align
  , i3bName :: Maybe Text
  , i3bInstance :: Maybe Text
  , i3bUrgent :: Bool
  , i3bSeparator :: Bool
  , i3bSeparatorBlockWidth :: Int
  , i3bMarkup :: Markup
  }

deriveToJSON deriveOptions ''I3BarBlock
makeLensesWith defaultFieldRules
  { _fieldToDef = \c fields field -> abbreviatedNamer c fields $
                                     case nameBase field of
                                       "i3bInstance" -> mkName "i3bInstanceName"
                                       _ -> field
      } ''I3BarBlock

-- | Default values of the block according to the protocol
defaultBlock :: I3BarBlock
defaultBlock = I3BarBlock
  { i3bFullText = ""
  , i3bShortText = Nothing
  , i3bColor = Nothing
  , i3bBackground = Nothing
  , i3bBorder = Nothing
  , i3bBorderBottom = 1
  , i3bBorderTop = 1
  , i3bBorderRight = 1
  , i3bBorderLeft = 1
  , i3bMinWidth = Nothing
  , i3bAlign = AlignLeft
  , i3bName = Nothing
  , i3bInstance = Nothing
  , i3bUrgent = False
  , i3bSeparator = True
  , i3bSeparatorBlockWidth = 9
  , i3bMarkup = NoMarkup
  }


data ClickEvent = ClickEvent
  { ceName :: Text
  , ceInstance :: Text
  , ceX :: Int
  , ceY :: Int
  , ceRelativeX :: Int
  , ceRelativeY :: Int
  , ceWidth :: Int
  , ceHeight :: Int
  , ceModifiers :: [Text]
  }

deriveFromJSON deriveOptions ''ClickEvent

