{-# LANGUAGE OverloadedStrings #-}
module Hloc.Color where

import Numeric(showHex)
import Data.Word
import Data.Aeson
import Data.Aeson.Encoding
import qualified Data.Text as T

type ColorPrim = Word8

data Color = RGB  !ColorPrim !ColorPrim !ColorPrim
           | RGBA !ColorPrim !ColorPrim !ColorPrim !ColorPrim

show2Hex :: ColorPrim -> String -> String
show2Hex x = if x < 16 then ('0':) . showHex x else showHex x
{-# INLINE show2Hex #-}

showColor :: Color -> String
showColor (RGB r g b) = '#':(show2Hex r . show2Hex g . show2Hex b) ""
showColor (RGBA r g b a) = '#':(show2Hex r . show2Hex g . show2Hex b . show2Hex a) ""
{-# INLINE showColor #-}

instance ToJSON Color where
  toJSON = String . T.pack . showColor
  toEncoding = string . showColor

redPrim :: Color -> ColorPrim
redPrim (RGB r _ _) = r
redPrim (RGBA r _ _ _) = r
{-# INLINE redPrim #-}

greenPrim :: Color -> ColorPrim
greenPrim (RGB _ g _) = g
greenPrim (RGBA _ g _ _) = g
{-# INLINE greenPrim #-}

bluePrim :: Color -> ColorPrim
bluePrim (RGB _ _ b) = b
bluePrim (RGBA _ _ b _) = b
{-# INLINE bluePrim #-}

alphaPrim :: Color -> ColorPrim
alphaPrim RGB{} = 255
alphaPrim (RGBA _ _ _ a) = a
{-# INLINE alphaPrim #-}

red :: Color
red = RGB 255 0 0
{-# INLINE red #-}

green :: Color
green = RGB 0 255 0
{-# INLINE green #-}

blue :: Color
blue = RGB 0 0 255
{-# INLINE blue #-}

boundSub :: ColorPrim -> ColorPrim -> ColorPrim
boundSub x y = if x < minBound + y then minBound else x - y
{-# INLINE boundSub #-}

boundAdd :: ColorPrim -> ColorPrim -> ColorPrim
boundAdd x y = if x > maxBound - y then maxBound else x + y
{-# INLINE boundAdd #-}

transp :: Color -> Color
transp (RGB r g b) = RGBA r g b 250
transp (RGBA r g b a) = RGBA r g b (boundSub a 5)
{-# INLINE transp #-}

dark :: Color -> Color
dark (RGB r g b) = RGB (boundSub r 5) (boundSub g 5) (boundSub b 5)
dark (RGBA r g b a) = RGBA (boundSub r 5) (boundSub g 5) (boundSub b 5) a
{-# INLINE dark #-}

light :: Color -> Color
light (RGB r g b) = RGB (boundAdd r 5) (boundAdd g 5) (boundAdd b 5)
light (RGBA r g b a) = RGBA (boundAdd r 5) (boundAdd g 5) (boundAdd b 5) a
{-# INLINE light #-}

transpX :: Int -> Color -> Color
transpX i = (!!i) . (iterate transp)
{-# INLINE transpX #-}

darkX :: Int -> Color -> Color
darkX i = (!!i) . (iterate dark)
{-# INLINE darkX #-}

lightX :: Int -> Color -> Color
lightX i = (!!i) . (iterate light)
{-# INLINE lightX #-}
