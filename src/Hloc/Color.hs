{-# LANGUAGE OverloadedStrings #-}
module Hloc.Color where

import Text.Printf (printf)
import Data.Word
import Data.Aeson

type ColorPrim = Word8

data Color = RGB ColorPrim ColorPrim ColorPrim
           | RGBA ColorPrim ColorPrim ColorPrim ColorPrim

instance ToJSON Color where
  toJSON (RGB r g b) = toJSON $ (printf "#%02x%02x%02x" r g b :: String)
  toJSON (RGBA r g b a) = toJSON $ (printf "#%02x%02x%02x%02x" r g b a :: String)


redPrim :: Color -> ColorPrim
redPrim (RGB r _ _) = r
redPrim (RGBA r _ _ _) = r

greenPrim :: Color -> ColorPrim
greenPrim (RGB _ g _) = g
greenPrim (RGBA _ g _ _) = g

bluePrim :: Color -> ColorPrim
bluePrim (RGB _ _ b) = b
bluePrim (RGBA _ _ b _) = b

alphaPrim :: Color -> ColorPrim
alphaPrim (RGB _ _ _) = 255
alphaPrim (RGBA _ _ _ a) = a

red :: Color
red = RGB 255 0 0

green :: Color
green = RGB 0 255 0

blue :: Color
blue = RGB 0 0 255

boundSub :: ColorPrim -> ColorPrim -> ColorPrim
boundSub x y = if x < minBound + y then minBound else x - y

boundAdd :: ColorPrim -> ColorPrim -> ColorPrim
boundAdd x y = if x > maxBound - y then maxBound else x + y

transp :: Color -> Color
transp (RGB r g b) = RGBA r g b 250
transp (RGBA r g b a) = RGBA r g b (boundSub a 5)

dark :: Color -> Color
dark (RGB r g b) = RGB (boundSub r 5) (boundSub g 5) (boundSub b 5)
dark (RGBA r g b a) = RGBA (boundSub r 5) (boundSub g 5) (boundSub b 5) a

light :: Color -> Color
light (RGB r g b) = RGB (boundAdd r 5) (boundAdd g 5) (boundAdd b 5)
light (RGBA r g b a) = RGBA (boundAdd r 5) (boundAdd g 5) (boundAdd b 5) a


transpX :: Int -> Color -> Color
transpX i = (!!i) . (iterate transp)

darkX :: Int -> Color -> Color
darkX i = (!!i) . (iterate dark)

lightX :: Int -> Color -> Color
lightX i = (!!i) . (iterate light)
