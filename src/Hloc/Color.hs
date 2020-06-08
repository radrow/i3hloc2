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
transpX i = (!!i) . iterate transp
{-# INLINE transpX #-}

darkX :: Int -> Color -> Color
darkX i = (!!i) . iterate dark
{-# INLINE darkX #-}

lightX :: Int -> Color -> Color
lightX i = (!!i) . iterate light
{-# INLINE lightX #-}


---- COLOR SAMPLES ----

black :: Color
black = RGB 0 0 0

white :: Color
white = RGB 255 255 255

red :: Color
red = RGB 255 0 0

lime :: Color
lime = RGB 0 255 0

blue :: Color
blue = RGB 0 0 255

yellow :: Color
yellow = RGB 255 255 0

cyan :: Color
cyan = RGB 0 255 255

magenta :: Color
magenta = RGB 255 0 255

silver :: Color
silver = RGB 192 192 192

gray :: Color
gray = RGB 128 128 128

maroon :: Color
maroon = RGB 128 0 0

olive :: Color
olive = RGB 128 128 0

green :: Color
green = RGB 0 128 0

purple :: Color
purple = RGB 128 0 128

teal :: Color
teal = RGB 0 128 128

navy :: Color
navy = RGB 0 0 128

brown :: Color
brown = RGB 165 42 42

firebrick :: Color
firebrick = RGB 178 34 34

crimson :: Color
crimson = RGB 220 20 60

tomato :: Color
tomato = RGB 255 99 71

coral :: Color
coral = RGB 255 127 80

indianRed :: Color
indianRed = RGB 205 92 92

salmon :: Color
salmon = RGB 250 128 114

orangeRed :: Color
orangeRed = RGB 255 69 0

orange :: Color
orange = RGB 255 165 0

gold :: Color
gold = RGB 255 215 0

goldenRod :: Color
goldenRod = RGB 218 165 32

paleGoldenRod :: Color
paleGoldenRod = RGB 238 232 170

khaki :: Color
khaki = RGB 240 230 140

yellowGreen :: Color
yellowGreen = RGB 154 205 50

oliveDrab :: Color
oliveDrab = RGB 107 142 35

lawnGreen :: Color
lawnGreen = RGB 124 252 0

chartReuse :: Color
chartReuse = RGB 127 255 0

greenYellow :: Color
greenYellow = RGB 173 255 47

forestGreen :: Color
forestGreen = RGB 34 139 34

limeGreen :: Color
limeGreen = RGB 50 205 50

paleGreen :: Color
paleGreen = RGB 152 251 152

mediumSpringGreen :: Color
mediumSpringGreen = RGB 0 250 154

springGreen :: Color
springGreen = RGB 0 255 127

seaGreen :: Color
seaGreen = RGB 46 139 87

mediumAquaMarine :: Color
mediumAquaMarine = RGB 102 205 170

mediumSeaGreen :: Color
mediumSeaGreen = RGB 60 179 113

aqua :: Color
aqua = RGB 0 255 255

turquoise :: Color
turquoise = RGB 64 224 208

mediumTurquoise :: Color
mediumTurquoise = RGB 72 209 204

paleTurquoise :: Color
paleTurquoise = RGB 175 238 238

aquaMarine :: Color
aquaMarine = RGB 127 255 212

powderBlue :: Color
powderBlue = RGB 176 224 230

cadetBlue :: Color
cadetBlue = RGB 95 158 160

steelBlue :: Color
steelBlue = RGB 70 130 180

cornFlowerBlue :: Color
cornFlowerBlue = RGB 100 149 237

deepSkyBlue :: Color
deepSkyBlue = RGB 0 191 255

dodgerBlue :: Color
dodgerBlue = RGB 30 144 255

skyBlue :: Color
skyBlue = RGB 135 206 235

midnightBlue :: Color
midnightBlue = RGB 25 25 112

mediumBlue :: Color
mediumBlue = RGB 0 0 205

royalBlue :: Color
royalBlue = RGB 65 105 225

blueViolet :: Color
blueViolet = RGB 138 43 226

indigo :: Color
indigo = RGB 75 0 130

slateBlue :: Color
slateBlue = RGB 106 90 205

mediumSlateBlue :: Color
mediumSlateBlue = RGB 123 104 238

mediumPurple :: Color
mediumPurple = RGB 147 112 219

mediumOrchid :: Color
mediumOrchid = RGB 186 85 211

thistle :: Color
thistle = RGB 216 191 216

plum :: Color
plum = RGB 221 160 221

violet :: Color
violet = RGB 238 130 238

orchid :: Color
orchid = RGB 218 112 214

mediumVioletRed :: Color
mediumVioletRed = RGB 199 21 133

paleVioletRed :: Color
paleVioletRed = RGB 219 112 147

deepPink :: Color
deepPink = RGB 255 20 147

hotPink :: Color
hotPink = RGB 255 105 180

pink :: Color
pink = RGB 255 192 203

antiqueWhite :: Color
antiqueWhite = RGB 250 235 215

beige :: Color
beige = RGB 245 245 220

bisque :: Color
bisque = RGB 255 228 196

blanchedAlmond :: Color
blanchedAlmond = RGB 255 235 205

wheat :: Color
wheat = RGB 245 222 179

cornSilk :: Color
cornSilk = RGB 255 248 220

lemonChiffon :: Color
lemonChiffon = RGB 255 250 205

saddleBrown :: Color
saddleBrown = RGB 139 69 19

sienna :: Color
sienna = RGB 160 82 45

chocolate :: Color
chocolate = RGB 210 105 30

peru :: Color
peru = RGB 205 133 63

sandyBrown :: Color
sandyBrown = RGB 244 164 96

burlyWood :: Color
burlyWood = RGB 222 184 135

tan :: Color
tan = RGB 210 180 140

rosyBrown :: Color
rosyBrown = RGB 188 143 143

moccasin :: Color
moccasin = RGB 255 228 181

navajoWhite :: Color
navajoWhite = RGB 255 222 173

peachPuff :: Color
peachPuff = RGB 255 218 185

mistyRose :: Color
mistyRose = RGB 255 228 225

lavenderBlush :: Color
lavenderBlush = RGB 255 240 245

linen :: Color
linen = RGB 250 240 230

oldLace :: Color
oldLace = RGB 253 245 230

papayaWhip :: Color
papayaWhip = RGB 255 239 213

seaShell :: Color
seaShell = RGB 255 245 238

mintCream :: Color
mintCream = RGB 245 255 250

slateGray :: Color
slateGray = RGB 112 128 144

lavender :: Color
lavender = RGB 230 230 250

floralWhite :: Color
floralWhite = RGB 255 250 240

aliceBlue :: Color
aliceBlue = RGB 240 248 255

ghostWhite :: Color
ghostWhite = RGB 248 248 255

honeydew :: Color
honeydew = RGB 240 255 240

ivory :: Color
ivory = RGB 255 255 240

azure :: Color
azure = RGB 240 255 255

snow :: Color
snow = RGB 255 250 250

gainsboro :: Color
gainsboro = RGB 220 220 220

whiteSmoke :: Color
whiteSmoke = RGB 245 245 245


